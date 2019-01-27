package models

final case class AccountBalanceNode(id: _root_.scala.Double, name: _root_.scala.Predef.String, balance: _root_.scala.Option[models.Money])

final case class AccountBalanceTree(value: models.AccountBalanceNode, children: _root_.scala.List[models.AccountBalanceTree])

final case class AccountBalances(children: _root_.scala.List[models.AccountBalanceTree])

final case class Money(value: _root_.scala.Double) extends _root_.scala.AnyVal
