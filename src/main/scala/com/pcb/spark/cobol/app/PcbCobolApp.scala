package com.pcb.spark.cobol.app

import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types.StringType
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.functions.col
import org.apache.spark.sql.types.IntegerType
import org.apache.spark.sql.types.StringType
import org.apache.spark.SparkContext

import scala.io.Source
// To execute the code : copy jar file to the Docker Spark Cluster - docker cp spark-cobol-app-assembly-0.1.0-SNAPSHOT.jar fab72f354f26:/appjars
//  /spark/bin/spark-submit  --master spark://spark-master:7077  --executor-memory 3G  --class com.pcb.spark.cobol.app.PcbCobolApp <JarFile.jar> 2>/dev/null
  object PcbCobolApp extends Serializable  {

    //Execute a given SQL against an oracle data base and return the resulting dataframe
    def execute_sql(sql: String): DataFrame = {
    val spark = SparkSession.builder.getOrCreate()
     val mysqlContext1 =  spark.sqlContext

      val result_df = mysqlContext1.read.format("jdbc")
        .option("url", "jdbc:oracle:thin:@oracle.apps.dev-001.nonprod.pcfcloud.io:31523/ORCLCDB.localdomain")
        .option("dbtable", sql)
        .option("user", "cardinal")
        .option("password", "passw0rd1")
        .load()

    return result_df
    }

    def main(args: Array[String]): Unit = {

      val spark = SparkSession
        .builder()
        .appName("PCB EBCDIC TEST APP VER 0.0.1")
        .getOrCreate()
      spark.sparkContext.setLogLevel("ERROR")

      import spark.implicits._
      //val sc = new SparkContext(new SparkConf().setMaster("spark://93e737cc1e46:7077").setAppName("PCB EBCIDIC CODE"))
      val mysqlContext = spark.sqlContext
      mysqlContext.setConf("spark.sql.shuffle.partitions", "40")
      spark.conf.set("spark.sql.autoBroadcastJoinThreshold", -1)

      printf("Spark Based EBCDIC parser - Ver 0.0.1\n")
      printf("**************************************\n")
      printf("\n\n")

      printf("Extracting account table data from CARDINAL.ACCOUNT\n")

      // Extract data from Oracle tables to spark dataframes
      val cardinal_account_sql = Source.fromFile("/cobrix-poc/sql/cardinalaccount.sql").mkString //read file
      val cardinal_account_df = execute_sql(cardinal_account_sql)
      cardinal_account_df.show(10,false)

      printf("Extracting statement cycle data table data from CARDINAL.STATEMENT_CYCLE\n")
      val statement_cycle_sql = Source.fromFile("/cobrix-poc/sql/statementcycle.sql").mkString //read file
      val statement_cycle_df = execute_sql(statement_cycle_sql)
      statement_cycle_df.show(10,false)

      printf("Extracting payment information data table data from CARDINAL.PAYMENT_INFORMATION\n")
      val payment_information_sql = Source.fromFile("/cobrix-poc/sql/paymentinformation.sql").mkString //read file
      val payment_information_df = execute_sql(payment_information_sql)
      payment_information_df.show(10,false)

      // READ EBCDIC File and Join with the tables from Database
      printf("Extracting EBCDIC Records using AH01 copybook layout\n")
      val tsys_account_master_list = spark
        .read
        .format("cobol")
        .option("copybook","/cobrix-poc/copybooks/AH01_REC.cpy")
        .option("encoding","EBCDIC")
        .option("schema_retention_policy","collapse_root")
        .option("generate_record_id", true)
        .load("/cobrix-poc/EBCDICData/")
      tsys_account_master_list.printSchema

      val tsys_account_master_header = spark
        .read
        .format("cobol")
        .option("copybook","/cobrix-poc/copybooks/HEADER.cpy")
        .option("encoding","EBCDIC")
        .option("schema_retention_policy","collapse_root")
        .option("generate_record_id", true)
        .load("/cobrix-poc/EBCDICData/")
      tsys_account_master_header.printSchema

      println("Read the AH01 Records, select columns, now filtering, Flattening the AH01 records\n")
      val ah01cycledata = tsys_account_master_list
        .filter($"MAST_REC_TYPE"==="AH01")
        .select("MAST_ACCOUNT_ID","MAST_REC_TYPE","AH01_ACT_CYCLE_HIST_INFO.AH01_ACT_HIST_CYCLE_KEY.AH01_CDATE_CYCLE","AH01_ACT_CYCLE_HIST_INFO.AH01_CYCLE_DATA.AH01_DATE_PAYMENT_DUE","AH01_ACT_CYCLE_HIST_INFO.AH01_CYCLE_DATA.AH01_CALC_MIN_PAYMENT","AH01_ACT_CYCLE_HIST_INFO.AH01_CYCLE_DATA.AH01_STMT_MIN_PAYMENT","AH01_ACT_CYCLE_HIST_INFO.AH01_CYCLE_DATA.AH01_BALANCE_CURRENT")

      println("Calculating number of duplicate rows \n")
      val ah01cycledata_duplicates = ah01cycledata.groupBy("MAST_ACCOUNT_ID")
        .count()
        .withColumnRenamed("count", "NumRecords")
          .filter("NumRecords>=2")

      // Addding the counts of records back to the dataframe for AH001
      val ah01cycledata_dup_counts = ah01cycledata.join(broadcast(ah01cycledata_duplicates),Seq("MAST_ACCOUNT_ID"),"left_outer")

      // Join account table
      println("Joining the TSYS-EBCDIC Data with the Oracle Cardinal.Accounts table  \n")
      val cardinal_tsys_match = cardinal_account_df.join(broadcast(ah01cycledata_dup_counts),cardinal_account_df("ACCOUNT_NO") === substring(ah01cycledata_dup_counts("MAST_ACCOUNT_ID"), -11, 11 ),"right")

      //Joining with the STATEMENT_CYCLE table
      println("Joining the TSYS-EBCDIC Data with the Oracle Cardinal.statementcycle table  \n")
      val cardinal_tsys_stmt_match = cardinal_tsys_match.join(broadcast(statement_cycle_df),Seq("ACCOUNT_UID"),"left_outer")

      //sc.setLocalProperty("callSite.short", "JoinPayment")
      println("Joining the TSYS-EBCDIC Data with the Oracle Cardinal.paymentcycle table  \n")
      val ah01_stmt_payment_df = cardinal_tsys_stmt_match.join(broadcast(payment_information_df),Seq("ACCOUNT_UID"),"left_outer")

      println("Extracting duplicate records only  \n")
      val ah01_stmt_payment_df_dups = ah01_stmt_payment_df.filter($"NumRecords">=2)

      println("Adding validation columns  \n")
      //Adding Validations to the Data Frame to allow for filtering in following section
      val ah01_stmt_payment_df_validated = ah01_stmt_payment_df
        .withColumn("validAH01_DATE_PAYMENT_DUE", when(to_date(col("AH01_DATE_PAYMENT_DUE").cast(StringType),"YYYYDDD").isNotNull, false).otherwise(true))
        .withColumn("validAH01_CDATE_CYCLE", when(to_date(col("AH01_CDATE_CYCLE").cast(StringType),"YYYYDDD").isNotNull, false).otherwise(true))
        .withColumn("START_DT_1MONTH", month(to_date(col("START_DT").cast(StringType),"yyyy-MM-dd HH:mm:ss")))
        .filter($"START_DT".isNotNull)

      println("Please check http://localhost:8080 to track progress of running job  \n")
      println("Please check http://localhost:4040 to track Event timelines of the job  \n")

      ah01_stmt_payment_df.printSchema()
      val jdbc1_df = ah01_stmt_payment_df.write.format("jdbc")
        .option("url", "jdbc:oracle:thin:@oracle.apps.dev-001.nonprod.pcfcloud.io:31523/ORCLCDB.localdomain")
        .option("dbtable", "SparkAH01")
        .option("user", "cardinal")
        .option("password", "passw0rd1")
        .mode("overwrite")
        .save()

      ah01_stmt_payment_df_validated.printSchema()
      val jdbc2_df = ah01_stmt_payment_df_validated.write.format("jdbc")
        .option("url", "jdbc:oracle:thin:@oracle.apps.dev-001.nonprod.pcfcloud.io:31523/ORCLCDB.localdomain")
        .option("dbtable", "SparkValidatedAH01")
        .option("user", "cardinal")
        .option("password", "passw0rd1")
        .mode("overwrite")
        .save()

      println("Job completed execution. Please check table SparkAH01, SparkValidatedAH01  \n")

    }
  }
