namespace * dke.tradesim.thrift

enum TransactionType {
  ORDER = 1,
  SPLIT_ADJUSTMENT = 2,
  CASH_DIVIDEND_PAYMENT = 3
}

struct MarketBuyOrder {
  1: i64 time,
  2: string symbol,
  3: i64 qty,
  4: optional string fillPrice
}

struct MarketSellOrder {
  1: i64 time,
  2: string symbol,
  3: i64 qty,
  4: optional string fillPrice
}

struct LimitBuyOrder {
  1: i64 time,
  2: string symbol,
  3: i64 qty,
  4: string limitPrice
  5: optional string fillPrice
}

struct LimitSellOrder {
  1: i64 time,
  2: string symbol,
  3: i64 qty,
  4: string limitPrice
  5: optional string fillPrice
}

struct SplitAdjustment {
  1: string symbol,
  2: i64 exDate,
  3: string ratio,
  4: i64 adjustmentTime,
  5: i64 shareQtyDelta,
  6: string cashPayout
}

struct CashDividendPayment {
  1: string symbol,
  2: i64 exDate,
  3: optional i64 payableDate
  4: string amountPerShare,
  5: i64 adjustmentTime,
  6: i64 shareQty,
  7: string total
}

struct Transaction {
  1: TransactionType type,
  2: optional MarketBuyOrder marketBuyOrder,
  3: optional MarketSellOrder marketSellOrder,
  4: optional LimitBuyOrder limitBuyOrder,
  5: optional LimitSellOrder limitSellOrder,
  6: optional SplitAdjustment splitAdjustment,
  7: optional CashDividendPayment cashDividendPayment
}

struct TransactionLog {
  1: list<Transaction> transactions
}

struct PortfolioValue {
  1: i64 time,
  2: string value
}

struct PortfolioValueHistory {
  1: list<PortfolioValue> portfolioValues
}

struct SymbolList {
  1: list<string> symbols
}