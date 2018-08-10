package scala.meta.tests.pretty

object LiteralSuite extends BaseScalaPrinterTest {
  check(
    """
      |foo('''
      |''')
      |""".stripMargin,
    """
      |foo(
      |  '''
      |'''
      |)""".stripMargin
  )
  check("'c'")
}
