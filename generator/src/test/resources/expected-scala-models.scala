case class AccountBalanceNode(id: Double, name: String, balance: Option[Money])
case class AccountBalanceTree(value: AccountBalanceNode, children: List[AccountBalanceTree])
case class AccountBalances(children: List[AccountBalanceTree])
case class Money(value: Double) extends AnyVal
