package controllers

import play.api._
import play.api.mvc._

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scala.concurrent.Future

import reactivemongo.api._
import reactivemongo.bson._

// Reactive Mongo plugin, including the JSON-specialized collection
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api.collections.default.BSONCollection
import play.modules.reactivemongo.json.BSONFormats._

case class User(id: String, firstName: String, lastName: String, age: Long)

object Application extends Controller with MongoController {

	implicit val rds = (
			(__ \ 'id).read[String] and
			(__ \ 'firstName).read[String] and
			(__ \ 'lastName).read[String] and
			(__ \ 'age).read[Long]
		)(User.apply _)

	def collection: JSONCollection = db.collection[JSONCollection]("persons")

	def index = Action {
		Ok("index")
	}

	def findByName(name: String) = Action.async {
		val cursor = collection.find(Json.obj("firstName" -> name)).cursor[JsObject]
		val futurePersonsList: Future[List[JsObject]] = cursor.collect[List]()
		
		val futurePersonsJsonArray: Future[JsArray] = futurePersonsList.map { persons =>
				Json.arr(persons)
			}
		
			futurePersonsJsonArray.map { persons =>
				Ok(persons)
			}
	}

	def update = Action.async(parse.json) { request => 
		import play.api.libs.json.Reads._

		request.body.validate[User].map { result =>
		
			val objectId = new BSONObjectID(result.id)
			
			val q = Json.obj("_id" -> objectId)

			val json = Json.obj(
				    "firstName" -> result.firstName,
						"lastName" -> result.lastName,
			      "age" -> result.age,
						"updated" -> new java.util.Date().getTime())

			collection.update(q, json).map { lastError =>
				Logger.debug(s"Successfully inserted with LastError: $lastError")
				Created
			}
		}.getOrElse(Future.successful(BadRequest("invalid json")))
		
	}

	def create = Action.async(parse.json) { request => 
		//using transformer
		import play.api.libs.json.Reads._

		val transformer: Reads[JsObject] =
			Reads.jsPickBranch[JsString](__ \ "firstName") and
			Reads.jsPickBranch[JsString](__ \ "lastName") and
			Reads.jsPickBranch[JsNumber](__ \ "age") reduce

		request.body.transform(transformer).map { result =>
			collection.insert(result).map { lastError =>
				Logger.debug(s"Successfully inserted with LastError: $lastError")
				Created
			}
		}.getOrElse(Future.successful(BadRequest("invalid json")))

	}
}
