import com.google.gson.Gson

import scala.util.matching.Regex
import org.apache.http.client.methods.HttpPost
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.DefaultHttpClient

object Submit {

  case class Player(name : String, score : Int){
    override def toString = s"$name ~ $score"
  }

  val leaderBoardUrl = "https://secret-hamlet-25480.herokuapp.com/db"

  def toLeaderBoard(name : String, score : Int) ={
    val clean: Regex = """\w""".r
    val sendName = clean.findAllIn(name).mkString

    val player = Player(sendName, score)

    println(player)

    val data = new Gson().toJson(player)

    postJson(leaderBoardUrl, data)
  }

  private def postJson(url: String, jsonData: String) = {

    val post = new HttpPost(url)
    post.setHeader("Content-type", "application/json")
    post.setEntity(new StringEntity(jsonData))
    (new DefaultHttpClient).execute(post)
  }


  private def get(url: String) = {
    scala.io.Source.fromURL(url).mkString
  }
}
