/*  / sfujimoto: 2017/07/09 20:03 */
import java.util.Date

object Four extends App {

  sealed trait Visitor {
    def id:String
    def createdAt:Date
    def age:Long = new Date().getTime() - createdAt.getTime()

  }

  final case class Anonymous(id:String, createdAt:Date = new Date()) extends Visitor

  final case class User(
                   id:String,
                   email:String,
                   createdAt:Date = new Date()
                 ) extends Visitor

  def missingCase(v: Visitor) =
    v match {
      case User(_, _, _) => "Got a user"
    }
}
