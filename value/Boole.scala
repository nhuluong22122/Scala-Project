package value
import expression._

case class Boole(val value: Boolean) extends Literal { //Boole class
  def &&(other: Boolean) = new Boole(this.value && other)
  def ||(other: Boolean) = new Boole(this.value || other)
  override def toString = value.toString
}