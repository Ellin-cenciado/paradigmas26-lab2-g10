import scala.io.Source
package NamedEntity{}
import NamedEntity._

// =====================================================================
// Ejercicio 2: Cargar diccionarios de entidades
// =====================================================================

/**
 * Responsable de cargar colecciones de entidades nombradas desde archivos.
 *
 * Un diccionario es un archivo de texto plano donde cada línea contiene
 * el nombre de una entidad conocida del mismo tipo.
 *
 * Ejemplo — data/people.txt:
 *   Martin Odersky
 *   Alan Turing
 *   Ada Lovelace
 *
 * Ejemplo — data/languages.txt:
 *   Scala
 *   Python
 *   Haskell
 */
object Dictionary {

  /**
   * Lee un archivo de diccionario y crea una lista de entidades del tipo indicado.
   *
   * @param filePath   ruta al archivo de diccionario (ej: "data/people.txt")
   * @param entityType tipo de entidad: "Person", "University", "ProgrammingLanguage", etc.
   * @return lista de NamedEntity del tipo correspondiente
   *
   * TODO (Ejercicio 2): Implementar este método.
   *
   *   Pasos sugeridos:
   *     1. Leer las líneas del archivo
   *     2. Para cada línea, crear la instancia de la clase correcta
   *     3. Retornar la lista de entidades creadas
   *
   *   Para crear la clase correcta según el tipo se puede usar match:
   *
   */
   def loadFromFile(filePath: String, entityType: String): List[NamedEntity] = {
     val source = Source.fromFile(filePath)
     var list = source.getLines().drop(1).toList

     val entity: String => NamedEntity = entityType match{
      case "Person" => (t: String) => new Person(t)
      case "Organization" => (t: String) => new Organization(t)
      case "University" => (t: String) => new University(t)
      case "Place" => (t: String) => new Place(t)
      case "Technology" => (t: String) => new Technology(t)
      case "ProgrammingLanguage" => (t: String) => new ProgrammingLanguage(t)
      case _ => {
      throw new IllegalArgumentException("Entity Type not Valid")
     }}
     var listaResultado = list.map(text=>entity(text))
     source.close()
     listaResultado
   }

  /**
   * Carga todos los diccionarios disponibles y combina sus entidades.
   *
   * @return lista con todas las entidades de todos los diccionarios
   *
   * TODO (Ejercicio 2): Implementar este método.
   *
   */
   def loadAll(): List[NamedEntity] = {
   loadFromFile("data/people.txt", "Person")
   loadFromFile("data/organizations.txt", "Organization")
   loadFromFile("data/languages.txt", "ProgrammingLanguage")
   loadFromFile("data/places.txt", "Place")
   loadFromFile("data/universities.txt", "University")
   }
}
