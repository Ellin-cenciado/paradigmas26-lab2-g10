import scala.util.matching.Regex
// =====================================================================
// Ejercicios 3 y 5: Detección y conteo de entidades
// =====================================================================

/**
 * Responsable de detectar entidades nombradas en texto libre y
 * producir estadísticas sobre ellas.
 */
object Analyzer {

  /**
   * Detecta las entidades del diccionario que aparecen en el texto dado.
   *
   * @param text       texto a analizar (ej: título o cuerpo de un post)
   * @param dictionary lista de entidades conocidas (cargadas desde los diccionarios)
   * @return lista de entidades cuyo texto aparece en el texto analizado
   *
   * TODO (Ejercicio 3): Implementar este método.
   *
   *   Para cada entidad en el diccionario, verificar si su texto aparece en el
   *   texto del post. Retornar únicamente las entidades que aparecen.
   *
   *   Ejemplo:
   *     text       = "Scala fue creado en EPFL por Martin Odersky"
   *     dictionary = List(
   *                    ProgrammingLanguage("Scala"),
   *                    University("EPFL"),
   *                    Person("Martin Odersky"),
   *                    Person("Ada Lovelace")   ← no aparece en el texto
   *                  )
   *     resultado  = List(
   *                    ProgrammingLanguage("Scala"),
   *                    University("EPFL"),
   *                    Person("Martin Odersky")
   *                  )
   */
  def detectEntities(text: String, dictionary: List[NamedEntity]): List[NamedEntity] = {
    dictionary.filter { e =>
      // Detecta si la entidad termina en caracter especial (como C++)
      // En ese caso no podemos usar lookahead al final porque el limite de palabra no aplica
      // Porque el caracter especial actua como limite
      val endsWithSpecial = e.text.lastOption.exists(!_.isLetterOrDigit)

      val pattern = if (endsWithSpecial)
        // (?i) indica que no importa las mayusculas
        // (?<![a-zA-Z0-9]) indica que no debe haber letra o digito antes de la entidad
        // Regex.quote escapa caracteres especiales del nombre (ej: + en C++)
        // Se salta el lookahead (?![a-zA-Z0-9])
        // .r indica que es una expresion regular
        s"(?i)(?<![a-zA-Z0-9])${Regex.quote(e.text)}".r
      else
        // (?![a-zA-Z0-9]) indica que no debe haber letra o digito despues
        s"(?i)(?<![a-zA-Z0-9])${Regex.quote(e.text)}(?![a-zA-Z0-9])".r

      // Devuelve true si encuentra al menos una ocurrencia en el texto
      pattern.findFirstIn(text).isDefined
    }
  }

  /**
   * Cuenta cuántas entidades de cada tipo fueron detectadas.
   *
   * @param entities lista de entidades detectadas
   * @return mapa de entityType → cantidad de apariciones
   *
   * TODO (Ejercicio 5): Implementar este método.
   *
   *   Ejemplo:
   *     entities = List(
   *                  Person("Alan Turing"),
   *                  ProgrammingLanguage("Scala"),
   *                  Person("Ada Lovelace"),
   *                  University("MIT")
   *                )
   *     resultado = Map(
   *                   "Person"              -> 2,
   *                   "ProgrammingLanguage" -> 1,
   *                   "University"          -> 1
   *                 )
   */
  def countByType(entities: List[NamedEntity]): Map[String, Int] = {
    // Agrupar por tipo y luego transformar la lista de objetos en su tamaño
    entities.groupBy(_.entityType).map { case (tipo, lista) => tipo -> lista.size }
  }
}
