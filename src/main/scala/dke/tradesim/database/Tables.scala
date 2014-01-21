package dke.tradesim.database
// AUTO-GENERATED Slick data model
/** Stand-alone Slick data model for immediate use */
object Tables extends {
  val profile = scala.slick.driver.PostgresDriver
} with Tables

/** Slick data model trait for extension, choice of backend or usage in the cake pattern. (Make sure to initialize this late.) */
trait Tables {
  val profile: scala.slick.driver.JdbcProfile
  import profile.simple._
  import scala.slick.model.ForeignKeyAction
  import scala.slick.jdbc.{GetResult => GR}
  // NOTE: GetResult mappers for plain SQL are only generated for tables where Slick knows how to map the types of all columns.
  
  /** Entity class storing rows of table AnnualReports
   *  @param id Database column id AutoInc, PrimaryKey
   *  @param startTime Database column start_time 
   *  @param endTime Database column end_time 
   *  @param publicationTime Database column publication_time 
   *  @param incomeStatement Database column income_statement 
   *  @param balanceSheet Database column balance_sheet 
   *  @param cashFlowStatement Database column cash_flow_statement 
   *  @param securityId Database column security_id  */
  case class AnnualReportsRow(id: Int, startTime: Long, endTime: Long, publicationTime: Long, incomeStatement: Array[Byte], balanceSheet: Array[Byte], cashFlowStatement: Array[Byte], securityId: Int)
  /** GetResult implicit for fetching AnnualReportsRow objects using plain SQL queries */
  implicit def GetResultAnnualReportsRow(implicit e0: GR[Int], e1: GR[Long], e2: GR[Array[Byte]]): GR[AnnualReportsRow] = GR{
    prs => import prs._
    AnnualReportsRow.tupled((<<[Int], <<[Long], <<[Long], <<[Long], <<[Array[Byte]], <<[Array[Byte]], <<[Array[Byte]], <<[Int]))
  }
  /** Table description of table annual_reports. Objects of this class serve as prototypes for rows in queries. */
  class AnnualReports(tag: Tag) extends Table[AnnualReportsRow](tag, "annual_reports") {
    def * = (id, startTime, endTime, publicationTime, incomeStatement, balanceSheet, cashFlowStatement, securityId) <> (AnnualReportsRow.tupled, AnnualReportsRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (id.?, startTime.?, endTime.?, publicationTime.?, incomeStatement.?, balanceSheet.?, cashFlowStatement.?, securityId.?).shaped.<>({r=>import r._; _1.map(_=> AnnualReportsRow.tupled((_1.get, _2.get, _3.get, _4.get, _5.get, _6.get, _7.get, _8.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column id AutoInc, PrimaryKey */
    val id: Column[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column start_time  */
    val startTime: Column[Long] = column[Long]("start_time")
    /** Database column end_time  */
    val endTime: Column[Long] = column[Long]("end_time")
    /** Database column publication_time  */
    val publicationTime: Column[Long] = column[Long]("publication_time")
    /** Database column income_statement  */
    val incomeStatement: Column[Array[Byte]] = column[Array[Byte]]("income_statement")
    /** Database column balance_sheet  */
    val balanceSheet: Column[Array[Byte]] = column[Array[Byte]]("balance_sheet")
    /** Database column cash_flow_statement  */
    val cashFlowStatement: Column[Array[Byte]] = column[Array[Byte]]("cash_flow_statement")
    /** Database column security_id  */
    val securityId: Column[Int] = column[Int]("security_id")
    
    /** Foreign key referencing Securities (database name annual_reports_security_id_fkey) */
    val securitiesFk = foreignKey("annual_reports_security_id_fkey", securityId, Securities)(r => r.id, onUpdate=ForeignKeyAction.NoAction, onDelete=ForeignKeyAction.NoAction)
    
    /** Index over (publicationTime) (database name annual_reports_publication_time_index) */
    val index1 = index("annual_reports_publication_time_index", publicationTime)
    /** Uniqueness Index over (securityId,endTime) (database name annual_reports_security_id_end_time_index) */
    val index2 = index("annual_reports_security_id_end_time_index", (securityId, endTime), unique=true)
  }
  /** Collection-like TableQuery object for table AnnualReports */
  lazy val AnnualReports = TableQuery[AnnualReports]
  
  /** Entity class storing rows of table CorporateActions
   *  @param id Database column id AutoInc, PrimaryKey
   *  @param `type` Database column type 
   *  @param exDate Database column ex_date 
   *  @param declarationDate Database column declaration_date 
   *  @param recordDate Database column record_date 
   *  @param payableDate Database column payable_date 
   *  @param number Database column number 
   *  @param securityId Database column security_id  */
  case class CorporateActionsRow(id: Int, `type`: String, exDate: Int, declarationDate: Option[Int], recordDate: Option[Int], payableDate: Option[Int], number: scala.math.BigDecimal, securityId: Int)
  /** GetResult implicit for fetching CorporateActionsRow objects using plain SQL queries */
  implicit def GetResultCorporateActionsRow(implicit e0: GR[Int], e1: GR[String], e2: GR[scala.math.BigDecimal]): GR[CorporateActionsRow] = GR{
    prs => import prs._
    CorporateActionsRow.tupled((<<[Int], <<[String], <<[Int], <<?[Int], <<?[Int], <<?[Int], <<[scala.math.BigDecimal], <<[Int]))
  }
  /** Table description of table corporate_actions. Objects of this class serve as prototypes for rows in queries.
   *  NOTE: The following names collided with Scala keywords and were escaped: type */
  class CorporateActions(tag: Tag) extends Table[CorporateActionsRow](tag, "corporate_actions") {
    def * = (id, `type`, exDate, declarationDate, recordDate, payableDate, number, securityId) <> (CorporateActionsRow.tupled, CorporateActionsRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (id.?, `type`.?, exDate.?, declarationDate, recordDate, payableDate, number.?, securityId.?).shaped.<>({r=>import r._; _1.map(_=> CorporateActionsRow.tupled((_1.get, _2.get, _3.get, _4, _5, _6, _7.get, _8.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column id AutoInc, PrimaryKey */
    val id: Column[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column type 
     *  NOTE: The name was escaped because it collided with a Scala keyword. */
    val `type`: Column[String] = column[String]("type")
    /** Database column ex_date  */
    val exDate: Column[Int] = column[Int]("ex_date")
    /** Database column declaration_date  */
    val declarationDate: Column[Option[Int]] = column[Option[Int]]("declaration_date")
    /** Database column record_date  */
    val recordDate: Column[Option[Int]] = column[Option[Int]]("record_date")
    /** Database column payable_date  */
    val payableDate: Column[Option[Int]] = column[Option[Int]]("payable_date")
    /** Database column number  */
    val number: Column[scala.math.BigDecimal] = column[scala.math.BigDecimal]("number")
    /** Database column security_id  */
    val securityId: Column[Int] = column[Int]("security_id")
    
    /** Foreign key referencing Securities (database name corporate_actions_security_id_fkey) */
    val securitiesFk = foreignKey("corporate_actions_security_id_fkey", securityId, Securities)(r => r.id, onUpdate=ForeignKeyAction.NoAction, onDelete=ForeignKeyAction.NoAction)
    
    /** Uniqueness Index over (`type`,securityId,exDate) (database name corporate_actions_type_security_id_ex_date_index) */
    val index1 = index("corporate_actions_type_security_id_ex_date_index", (`type`, securityId, exDate), unique=true)
  }
  /** Collection-like TableQuery object for table CorporateActions */
  lazy val CorporateActions = TableQuery[CorporateActions]
  
  /** Entity class storing rows of table EodBars
   *  @param id Database column id AutoInc, PrimaryKey
   *  @param startTime Database column start_time 
   *  @param endTime Database column end_time 
   *  @param open Database column open 
   *  @param high Database column high 
   *  @param low Database column low 
   *  @param close Database column close 
   *  @param volume Database column volume 
   *  @param securityId Database column security_id  */
  case class EodBarsRow(id: Int, startTime: Long, endTime: Long, open: scala.math.BigDecimal, high: scala.math.BigDecimal, low: scala.math.BigDecimal, close: scala.math.BigDecimal, volume: Long, securityId: Int)
  /** GetResult implicit for fetching EodBarsRow objects using plain SQL queries */
  implicit def GetResultEodBarsRow(implicit e0: GR[Int], e1: GR[Long], e2: GR[scala.math.BigDecimal]): GR[EodBarsRow] = GR{
    prs => import prs._
    EodBarsRow.tupled((<<[Int], <<[Long], <<[Long], <<[scala.math.BigDecimal], <<[scala.math.BigDecimal], <<[scala.math.BigDecimal], <<[scala.math.BigDecimal], <<[Long], <<[Int]))
  }
  /** Table description of table eod_bars. Objects of this class serve as prototypes for rows in queries. */
  class EodBars(tag: Tag) extends Table[EodBarsRow](tag, "eod_bars") {
    def * = (id, startTime, endTime, open, high, low, close, volume, securityId) <> (EodBarsRow.tupled, EodBarsRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (id.?, startTime.?, endTime.?, open.?, high.?, low.?, close.?, volume.?, securityId.?).shaped.<>({r=>import r._; _1.map(_=> EodBarsRow.tupled((_1.get, _2.get, _3.get, _4.get, _5.get, _6.get, _7.get, _8.get, _9.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column id AutoInc, PrimaryKey */
    val id: Column[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column start_time  */
    val startTime: Column[Long] = column[Long]("start_time")
    /** Database column end_time  */
    val endTime: Column[Long] = column[Long]("end_time")
    /** Database column open  */
    val open: Column[scala.math.BigDecimal] = column[scala.math.BigDecimal]("open")
    /** Database column high  */
    val high: Column[scala.math.BigDecimal] = column[scala.math.BigDecimal]("high")
    /** Database column low  */
    val low: Column[scala.math.BigDecimal] = column[scala.math.BigDecimal]("low")
    /** Database column close  */
    val close: Column[scala.math.BigDecimal] = column[scala.math.BigDecimal]("close")
    /** Database column volume  */
    val volume: Column[Long] = column[Long]("volume")
    /** Database column security_id  */
    val securityId: Column[Int] = column[Int]("security_id")
    
    /** Foreign key referencing Securities (database name eod_bars_security_id_fkey) */
    val securitiesFk = foreignKey("eod_bars_security_id_fkey", securityId, Securities)(r => r.id, onUpdate=ForeignKeyAction.NoAction, onDelete=ForeignKeyAction.NoAction)
    
    /** Uniqueness Index over (securityId,startTime) (database name eod_bars_security_id_start_time_index) */
    val index1 = index("eod_bars_security_id_start_time_index", (securityId, startTime), unique=true)
  }
  /** Collection-like TableQuery object for table EodBars */
  lazy val EodBars = TableQuery[EodBars]
  
  /** Entity class storing rows of table Exchanges
   *  @param id Database column id AutoInc, PrimaryKey
   *  @param label Database column label 
   *  @param name Database column name  */
  case class ExchangesRow(id: Int, label: String, name: Option[String])
  /** GetResult implicit for fetching ExchangesRow objects using plain SQL queries */
  implicit def GetResultExchangesRow(implicit e0: GR[Int], e1: GR[String]): GR[ExchangesRow] = GR{
    prs => import prs._
    ExchangesRow.tupled((<<[Int], <<[String], <<?[String]))
  }
  /** Table description of table exchanges. Objects of this class serve as prototypes for rows in queries. */
  class Exchanges(tag: Tag) extends Table[ExchangesRow](tag, "exchanges") {
    def * = (id, label, name) <> (ExchangesRow.tupled, ExchangesRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (id.?, label.?, name).shaped.<>({r=>import r._; _1.map(_=> ExchangesRow.tupled((_1.get, _2.get, _3)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column id AutoInc, PrimaryKey */
    val id: Column[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column label  */
    val label: Column[String] = column[String]("label")
    /** Database column name  */
    val name: Column[Option[String]] = column[Option[String]]("name")
    
    /** Uniqueness Index over (label) (database name exchanges_label_index) */
    val index1 = index("exchanges_label_index", label, unique=true)
  }
  /** Collection-like TableQuery object for table Exchanges */
  lazy val Exchanges = TableQuery[Exchanges]
  
  /** Entity class storing rows of table Industries
   *  @param id Database column id AutoInc, PrimaryKey
   *  @param name Database column name  */
  case class IndustriesRow(id: Int, name: String)
  /** GetResult implicit for fetching IndustriesRow objects using plain SQL queries */
  implicit def GetResultIndustriesRow(implicit e0: GR[Int], e1: GR[String]): GR[IndustriesRow] = GR{
    prs => import prs._
    IndustriesRow.tupled((<<[Int], <<[String]))
  }
  /** Table description of table industries. Objects of this class serve as prototypes for rows in queries. */
  class Industries(tag: Tag) extends Table[IndustriesRow](tag, "industries") {
    def * = (id, name) <> (IndustriesRow.tupled, IndustriesRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (id.?, name.?).shaped.<>({r=>import r._; _1.map(_=> IndustriesRow.tupled((_1.get, _2.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column id AutoInc, PrimaryKey */
    val id: Column[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column name  */
    val name: Column[String] = column[String]("name")
    
    /** Uniqueness Index over (name) (database name industries_name_index) */
    val index1 = index("industries_name_index", name, unique=true)
  }
  /** Collection-like TableQuery object for table Industries */
  lazy val Industries = TableQuery[Industries]
  
  /** Entity class storing rows of table QuarterlyReports
   *  @param id Database column id AutoInc, PrimaryKey
   *  @param startTime Database column start_time 
   *  @param endTime Database column end_time 
   *  @param publicationTime Database column publication_time 
   *  @param incomeStatement Database column income_statement 
   *  @param balanceSheet Database column balance_sheet 
   *  @param cashFlowStatement Database column cash_flow_statement 
   *  @param securityId Database column security_id  */
  case class QuarterlyReportsRow(id: Int, startTime: Long, endTime: Long, publicationTime: Long, incomeStatement: Array[Byte], balanceSheet: Array[Byte], cashFlowStatement: Array[Byte], securityId: Int)
  /** GetResult implicit for fetching QuarterlyReportsRow objects using plain SQL queries */
  implicit def GetResultQuarterlyReportsRow(implicit e0: GR[Int], e1: GR[Long], e2: GR[Array[Byte]]): GR[QuarterlyReportsRow] = GR{
    prs => import prs._
    QuarterlyReportsRow.tupled((<<[Int], <<[Long], <<[Long], <<[Long], <<[Array[Byte]], <<[Array[Byte]], <<[Array[Byte]], <<[Int]))
  }
  /** Table description of table quarterly_reports. Objects of this class serve as prototypes for rows in queries. */
  class QuarterlyReports(tag: Tag) extends Table[QuarterlyReportsRow](tag, "quarterly_reports") {
    def * = (id, startTime, endTime, publicationTime, incomeStatement, balanceSheet, cashFlowStatement, securityId) <> (QuarterlyReportsRow.tupled, QuarterlyReportsRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (id.?, startTime.?, endTime.?, publicationTime.?, incomeStatement.?, balanceSheet.?, cashFlowStatement.?, securityId.?).shaped.<>({r=>import r._; _1.map(_=> QuarterlyReportsRow.tupled((_1.get, _2.get, _3.get, _4.get, _5.get, _6.get, _7.get, _8.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column id AutoInc, PrimaryKey */
    val id: Column[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column start_time  */
    val startTime: Column[Long] = column[Long]("start_time")
    /** Database column end_time  */
    val endTime: Column[Long] = column[Long]("end_time")
    /** Database column publication_time  */
    val publicationTime: Column[Long] = column[Long]("publication_time")
    /** Database column income_statement  */
    val incomeStatement: Column[Array[Byte]] = column[Array[Byte]]("income_statement")
    /** Database column balance_sheet  */
    val balanceSheet: Column[Array[Byte]] = column[Array[Byte]]("balance_sheet")
    /** Database column cash_flow_statement  */
    val cashFlowStatement: Column[Array[Byte]] = column[Array[Byte]]("cash_flow_statement")
    /** Database column security_id  */
    val securityId: Column[Int] = column[Int]("security_id")
    
    /** Foreign key referencing Securities (database name quarterly_reports_security_id_fkey) */
    val securitiesFk = foreignKey("quarterly_reports_security_id_fkey", securityId, Securities)(r => r.id, onUpdate=ForeignKeyAction.NoAction, onDelete=ForeignKeyAction.NoAction)
    
    /** Index over (publicationTime) (database name quarterly_reports_publication_time_index) */
    val index1 = index("quarterly_reports_publication_time_index", publicationTime)
    /** Uniqueness Index over (securityId,endTime) (database name quarterly_reports_security_id_end_time_index) */
    val index2 = index("quarterly_reports_security_id_end_time_index", (securityId, endTime), unique=true)
  }
  /** Collection-like TableQuery object for table QuarterlyReports */
  lazy val QuarterlyReports = TableQuery[QuarterlyReports]
  
  /** Entity class storing rows of table SchemaInfo
   *  @param version Database column version Default(0) */
  case class SchemaInfoRow(version: Int=0)
  /** GetResult implicit for fetching SchemaInfoRow objects using plain SQL queries */
  implicit def GetResultSchemaInfoRow(implicit e0: GR[Int]): GR[SchemaInfoRow] = GR{
    prs => import prs._
    SchemaInfoRow(<<[Int])
  }
  /** Table description of table schema_info. Objects of this class serve as prototypes for rows in queries. */
  class SchemaInfo(tag: Tag) extends Table[SchemaInfoRow](tag, "schema_info") {
    def * = version <> (SchemaInfoRow, SchemaInfoRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = version.?.shaped.<>({r => r.map(version=> SchemaInfoRow(version))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column version Default(0) */
    val version: Column[Int] = column[Int]("version", O.Default(0))
  }
  /** Collection-like TableQuery object for table SchemaInfo */
  lazy val SchemaInfo = TableQuery[SchemaInfo]
  
  /** Entity class storing rows of table Sectors
   *  @param id Database column id AutoInc, PrimaryKey
   *  @param name Database column name  */
  case class SectorsRow(id: Int, name: String)
  /** GetResult implicit for fetching SectorsRow objects using plain SQL queries */
  implicit def GetResultSectorsRow(implicit e0: GR[Int], e1: GR[String]): GR[SectorsRow] = GR{
    prs => import prs._
    SectorsRow.tupled((<<[Int], <<[String]))
  }
  /** Table description of table sectors. Objects of this class serve as prototypes for rows in queries. */
  class Sectors(tag: Tag) extends Table[SectorsRow](tag, "sectors") {
    def * = (id, name) <> (SectorsRow.tupled, SectorsRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (id.?, name.?).shaped.<>({r=>import r._; _1.map(_=> SectorsRow.tupled((_1.get, _2.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column id AutoInc, PrimaryKey */
    val id: Column[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column name  */
    val name: Column[String] = column[String]("name")
    
    /** Uniqueness Index over (name) (database name sectors_name_index) */
    val index1 = index("sectors_name_index", name, unique=true)
  }
  /** Collection-like TableQuery object for table Sectors */
  lazy val Sectors = TableQuery[Sectors]
  
  /** Entity class storing rows of table Securities
   *  @param id Database column id AutoInc, PrimaryKey
   *  @param `type` Database column type 
   *  @param bbGid Database column bb_gid 
   *  @param bbGcid Database column bb_gcid 
   *  @param symbol Database column symbol 
   *  @param name Database column name 
   *  @param startDate Database column start_date 
   *  @param endDate Database column end_date 
   *  @param cik Database column cik 
   *  @param fiscalYearEndDate Database column fiscal_year_end_date 
   *  @param active Database column active Default(false)
   *  @param exchangeId Database column exchange_id 
   *  @param industryId Database column industry_id 
   *  @param sectorId Database column sector_id  */
  case class SecuritiesRow(id: Int, `type`: String, bbGid: Option[String], bbGcid: Option[String], symbol: String, name: Option[String], startDate: Option[Int], endDate: Option[Int], cik: Option[Int], fiscalYearEndDate: Option[Int], active: Option[Boolean]=Some(false), exchangeId: Option[Int], industryId: Option[Int], sectorId: Option[Int])
  /** GetResult implicit for fetching SecuritiesRow objects using plain SQL queries */
  implicit def GetResultSecuritiesRow(implicit e0: GR[Int], e1: GR[String], e2: GR[Boolean]): GR[SecuritiesRow] = GR{
    prs => import prs._
    SecuritiesRow.tupled((<<[Int], <<[String], <<?[String], <<?[String], <<[String], <<?[String], <<?[Int], <<?[Int], <<?[Int], <<?[Int], <<?[Boolean], <<?[Int], <<?[Int], <<?[Int]))
  }
  /** Table description of table securities. Objects of this class serve as prototypes for rows in queries.
   *  NOTE: The following names collided with Scala keywords and were escaped: type */
  class Securities(tag: Tag) extends Table[SecuritiesRow](tag, "securities") {
    def * = (id, `type`, bbGid, bbGcid, symbol, name, startDate, endDate, cik, fiscalYearEndDate, active, exchangeId, industryId, sectorId) <> (SecuritiesRow.tupled, SecuritiesRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (id.?, `type`.?, bbGid, bbGcid, symbol.?, name, startDate, endDate, cik, fiscalYearEndDate, active, exchangeId, industryId, sectorId).shaped.<>({r=>import r._; _1.map(_=> SecuritiesRow.tupled((_1.get, _2.get, _3, _4, _5.get, _6, _7, _8, _9, _10, _11, _12, _13, _14)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column id AutoInc, PrimaryKey */
    val id: Column[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column type 
     *  NOTE: The name was escaped because it collided with a Scala keyword. */
    val `type`: Column[String] = column[String]("type")
    /** Database column bb_gid  */
    val bbGid: Column[Option[String]] = column[Option[String]]("bb_gid")
    /** Database column bb_gcid  */
    val bbGcid: Column[Option[String]] = column[Option[String]]("bb_gcid")
    /** Database column symbol  */
    val symbol: Column[String] = column[String]("symbol")
    /** Database column name  */
    val name: Column[Option[String]] = column[Option[String]]("name")
    /** Database column start_date  */
    val startDate: Column[Option[Int]] = column[Option[Int]]("start_date")
    /** Database column end_date  */
    val endDate: Column[Option[Int]] = column[Option[Int]]("end_date")
    /** Database column cik  */
    val cik: Column[Option[Int]] = column[Option[Int]]("cik")
    /** Database column fiscal_year_end_date  */
    val fiscalYearEndDate: Column[Option[Int]] = column[Option[Int]]("fiscal_year_end_date")
    /** Database column active Default(false) */
    val active: Column[Option[Boolean]] = column[Option[Boolean]]("active", O.Default(Some(false)))
    /** Database column exchange_id  */
    val exchangeId: Column[Option[Int]] = column[Option[Int]]("exchange_id")
    /** Database column industry_id  */
    val industryId: Column[Option[Int]] = column[Option[Int]]("industry_id")
    /** Database column sector_id  */
    val sectorId: Column[Option[Int]] = column[Option[Int]]("sector_id")
    
    /** Foreign key referencing Exchanges (database name securities_exchange_id_fkey) */
    val exchangesFk = foreignKey("securities_exchange_id_fkey", exchangeId, Exchanges)(r => r.id, onUpdate=ForeignKeyAction.NoAction, onDelete=ForeignKeyAction.NoAction)
    /** Foreign key referencing Industries (database name securities_industry_id_fkey) */
    val industriesFk = foreignKey("securities_industry_id_fkey", industryId, Industries)(r => r.id, onUpdate=ForeignKeyAction.NoAction, onDelete=ForeignKeyAction.NoAction)
    /** Foreign key referencing Sectors (database name securities_sector_id_fkey) */
    val sectorsFk = foreignKey("securities_sector_id_fkey", sectorId, Sectors)(r => r.id, onUpdate=ForeignKeyAction.NoAction, onDelete=ForeignKeyAction.NoAction)
    
    /** Index over (bbGcid) (database name securities_bb_gcid_index) */
    val index1 = index("securities_bb_gcid_index", bbGcid)
    /** Uniqueness Index over (bbGid) (database name securities_bb_gid_index) */
    val index2 = index("securities_bb_gid_index", bbGid, unique=true)
    /** Index over (symbol) (database name securities_symbol_index) */
    val index3 = index("securities_symbol_index", symbol)
  }
  /** Collection-like TableQuery object for table Securities */
  lazy val Securities = TableQuery[Securities]
  
  /** Entity class storing rows of table SecuritiesTrialSets
   *  @param securityId Database column security_id 
   *  @param trialSetId Database column trial_set_id  */
  case class SecuritiesTrialSetsRow(securityId: Int, trialSetId: Int)
  /** GetResult implicit for fetching SecuritiesTrialSetsRow objects using plain SQL queries */
  implicit def GetResultSecuritiesTrialSetsRow(implicit e0: GR[Int]): GR[SecuritiesTrialSetsRow] = GR{
    prs => import prs._
    SecuritiesTrialSetsRow.tupled((<<[Int], <<[Int]))
  }
  /** Table description of table securities_trial_sets. Objects of this class serve as prototypes for rows in queries. */
  class SecuritiesTrialSets(tag: Tag) extends Table[SecuritiesTrialSetsRow](tag, "securities_trial_sets") {
    def * = (securityId, trialSetId) <> (SecuritiesTrialSetsRow.tupled, SecuritiesTrialSetsRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (securityId.?, trialSetId.?).shaped.<>({r=>import r._; _1.map(_=> SecuritiesTrialSetsRow.tupled((_1.get, _2.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column security_id  */
    val securityId: Column[Int] = column[Int]("security_id")
    /** Database column trial_set_id  */
    val trialSetId: Column[Int] = column[Int]("trial_set_id")
    
    /** Primary key of SecuritiesTrialSets (database name securities_trial_sets_pkey) */
    val pk = primaryKey("securities_trial_sets_pkey", (securityId, trialSetId))
    
    /** Foreign key referencing Securities (database name securities_trial_sets_security_id_fkey) */
    val securitiesFk = foreignKey("securities_trial_sets_security_id_fkey", securityId, Securities)(r => r.id, onUpdate=ForeignKeyAction.NoAction, onDelete=ForeignKeyAction.NoAction)
    /** Foreign key referencing TrialSets (database name securities_trial_sets_trial_set_id_fkey) */
    val trialSetsFk = foreignKey("securities_trial_sets_trial_set_id_fkey", trialSetId, TrialSets)(r => r.id, onUpdate=ForeignKeyAction.NoAction, onDelete=ForeignKeyAction.NoAction)
    
    /** Index over (trialSetId,securityId) (database name securities_trial_sets_trial_set_id_security_id_index) */
    val index1 = index("securities_trial_sets_trial_set_id_security_id_index", (trialSetId, securityId))
  }
  /** Collection-like TableQuery object for table SecuritiesTrialSets */
  lazy val SecuritiesTrialSets = TableQuery[SecuritiesTrialSets]
  
  /** Entity class storing rows of table Strategies
   *  @param id Database column id AutoInc, PrimaryKey
   *  @param name Database column name  */
  case class StrategiesRow(id: Int, name: String)
  /** GetResult implicit for fetching StrategiesRow objects using plain SQL queries */
  implicit def GetResultStrategiesRow(implicit e0: GR[Int], e1: GR[String]): GR[StrategiesRow] = GR{
    prs => import prs._
    StrategiesRow.tupled((<<[Int], <<[String]))
  }
  /** Table description of table strategies. Objects of this class serve as prototypes for rows in queries. */
  class Strategies(tag: Tag) extends Table[StrategiesRow](tag, "strategies") {
    def * = (id, name) <> (StrategiesRow.tupled, StrategiesRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (id.?, name.?).shaped.<>({r=>import r._; _1.map(_=> StrategiesRow.tupled((_1.get, _2.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column id AutoInc, PrimaryKey */
    val id: Column[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column name  */
    val name: Column[String] = column[String]("name")
    
    /** Uniqueness Index over (name) (database name strategies_name_index) */
    val index1 = index("strategies_name_index", name, unique=true)
  }
  /** Collection-like TableQuery object for table Strategies */
  lazy val Strategies = TableQuery[Strategies]
  
  /** Entity class storing rows of table Trials
   *  @param id Database column id AutoInc, PrimaryKey
   *  @param startTime Database column start_time 
   *  @param endTime Database column end_time 
   *  @param duration Database column duration
   *  @param transactionLog Database column transaction_log
   *  @param portfolioValueLog Database column portfolio_value_log 
   *  @param trialSetId Database column trial_set_id  */
  case class TrialsRow(id: Int, startTime: Long, endTime: Long, duration: String, transactionLog: Array[Byte], portfolioValueLog: Array[Byte], trialSetId: Int)
  /** GetResult implicit for fetching TrialsRow objects using plain SQL queries */
  implicit def GetResultTrialsRow(implicit e0: GR[Int], e1: GR[Long], e2: GR[Array[Byte]]): GR[TrialsRow] = GR{
    prs => import prs._
    TrialsRow.tupled((<<[Int], <<[Long], <<[Long], <<[String], <<[Array[Byte]], <<[Array[Byte]], <<[Int]))
  }
  /** Table description of table trials. Objects of this class serve as prototypes for rows in queries. */
  class Trials(tag: Tag) extends Table[TrialsRow](tag, "trials") {
    def * = (id, startTime, endTime, duration, transactionLog, portfolioValueLog, trialSetId) <> (TrialsRow.tupled, TrialsRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (id.?, startTime.?, endTime.?, duration.?, transactionLog.?, portfolioValueLog.?, trialSetId.?).shaped.<>({r=>import r._; _1.map(_=> TrialsRow.tupled((_1.get, _2.get, _3.get, _4.get, _5.get, _6.get, _7.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column id AutoInc, PrimaryKey */
    val id: Column[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column start_time  */
    val startTime: Column[Long] = column[Long]("start_time")
    /** Database column end_time  */
    val endTime: Column[Long] = column[Long]("end_time")
    /** Database column duration  */
    val duration: Column[String] = column[String]("duration")
    /** Database column transaction_log  */
    val transactionLog: Column[Array[Byte]] = column[Array[Byte]]("transaction_log")
    /** Database column portfolio_value_log  */
    val portfolioValueLog: Column[Array[Byte]] = column[Array[Byte]]("portfolio_value_log")
    /** Database column trial_set_id  */
    val trialSetId: Column[Int] = column[Int]("trial_set_id")
    
    /** Foreign key referencing TrialSets (database name trials_trial_set_id_fkey) */
    val trialSetsFk = foreignKey("trials_trial_set_id_fkey", trialSetId, TrialSets)(r => r.id, onUpdate=ForeignKeyAction.NoAction, onDelete=ForeignKeyAction.NoAction)
  }
  /** Collection-like TableQuery object for table Trials */
  lazy val Trials = TableQuery[Trials]
  
  /** Entity class storing rows of table TrialSets
   *  @param id Database column id AutoInc, PrimaryKey
   *  @param principal Database column principal 
   *  @param commissionPerTrade Database column commission_per_trade 
   *  @param commissionPerShare Database column commission_per_share 
   *  @param strategyId Database column strategy_id  */
  case class TrialSetsRow(id: Int, principal: Option[scala.math.BigDecimal], commissionPerTrade: Option[scala.math.BigDecimal], commissionPerShare: Option[scala.math.BigDecimal], strategyId: Int)
  /** GetResult implicit for fetching TrialSetsRow objects using plain SQL queries */
  implicit def GetResultTrialSetsRow(implicit e0: GR[Int], e1: GR[scala.math.BigDecimal]): GR[TrialSetsRow] = GR{
    prs => import prs._
    TrialSetsRow.tupled((<<[Int], <<?[scala.math.BigDecimal], <<?[scala.math.BigDecimal], <<?[scala.math.BigDecimal], <<[Int]))
  }
  /** Table description of table trial_sets. Objects of this class serve as prototypes for rows in queries. */
  class TrialSets(tag: Tag) extends Table[TrialSetsRow](tag, "trial_sets") {
    def * = (id, principal, commissionPerTrade, commissionPerShare, strategyId) <> (TrialSetsRow.tupled, TrialSetsRow.unapply)
    /** Maps whole row to an option. Useful for outer joins. */
    def ? = (id.?, principal, commissionPerTrade, commissionPerShare, strategyId.?).shaped.<>({r=>import r._; _1.map(_=> TrialSetsRow.tupled((_1.get, _2, _3, _4, _5.get)))}, (_:Any) =>  throw new Exception("Inserting into ? projection not supported."))
    
    /** Database column id AutoInc, PrimaryKey */
    val id: Column[Int] = column[Int]("id", O.AutoInc, O.PrimaryKey)
    /** Database column principal  */
    val principal: Column[Option[scala.math.BigDecimal]] = column[Option[scala.math.BigDecimal]]("principal")
    /** Database column commission_per_trade  */
    val commissionPerTrade: Column[Option[scala.math.BigDecimal]] = column[Option[scala.math.BigDecimal]]("commission_per_trade")
    /** Database column commission_per_share  */
    val commissionPerShare: Column[Option[scala.math.BigDecimal]] = column[Option[scala.math.BigDecimal]]("commission_per_share")
    /** Database column strategy_id  */
    val strategyId: Column[Int] = column[Int]("strategy_id")
    
    /** Foreign key referencing Strategies (database name trial_sets_strategy_id_fkey) */
    val strategiesFk = foreignKey("trial_sets_strategy_id_fkey", strategyId, Strategies)(r => r.id, onUpdate=ForeignKeyAction.NoAction, onDelete=ForeignKeyAction.NoAction)
  }
  /** Collection-like TableQuery object for table TrialSets */
  lazy val TrialSets = TableQuery[TrialSets]
}