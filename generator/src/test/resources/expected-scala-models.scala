package models

final case class AccountBalanceNode(id: Double, name: String, balance: Option[Money])
final case class AccountBalanceTree(value: AccountBalanceNode, children: List[AccountBalanceTree])
final case class AccountBalances(children: List[AccountBalanceTree])
final case class Money(value: Double) extends AnyVal
