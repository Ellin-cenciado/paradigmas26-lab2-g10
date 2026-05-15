// =====================================================================
// Ejercicios 4 y 5: Formateo de resultados
// =====================================================================

/**
 * Responsable de convertir los resultados del análisis a texto para mostrar.
 */
object Formatters {

  /**
   * Formatea el análisis NER de un post individual.
   *
   * @param postTitle título del post analizado
   * @param entities  entidades detectadas en ese post
   * @return bloque de texto con el título y las entidades encontradas
   *
   * TODO (Ejercicio 4): Implementar este método.
   *
   *   Usar el método describe de cada entidad para generar la salida.
   *   No es necesario hacer match sobre el tipo concreto de cada entidad:
   *   describe ya funciona correctamente para cualquier subtipo (polimorfismo).
   *
   *   Ejemplo de salida esperada:
   *
   *     Post: "Scala 3 released at EPFL by Martin Odersky"
   *     Entidades detectadas:
   *       [ProgrammingLanguage] Scala
   *       [University] EPFL
   *       [Person] Martin Odersky
   *
   *   Si no se detectaron entidades, mostrar un mensaje indicándolo.
   */
  def formatNERResult(postTitle: String, entities: List[NamedEntity]): String = {
    var result = s"Post: \"${postTitle}\"\n"

    if (entities.nonEmpty) {
        result += "Entidades detectadas:\n"
        for (entity <- entities) {
            result += s"\t${entity.describe}\n"
        }
    } else {
        result += "\t(sin entidades detectadas)\n"
    }

    return result
  }

  /**
   * Formatea un resumen de estadísticas de entidades por tipo.
   *
   * @param counts mapa de entityType → cantidad
   * @return texto con las estadísticas ordenadas por cantidad (de mayor a menor)
   *
   * TODO (Ejercicio 5): Implementar este método.
   *
   *   Ejemplo de salida esperada:
   *
   *     === Estadísticas de entidades ===
   *     Person: 5
   *     ProgrammingLanguage: 3
   *     Organization: 2
   *     University: 2
   */
  def formatEntityStats(counts: Map[String, Int]): String = {
    val header = "=== Estadísticas de entidades ==="
    val lines = counts.toList
      .sortBy { case (_, count) => -count }   // mayor a menor
      .map    { case (tipo, count) => s"$tipo: $count" } // Transforma cada par en una linea de texto
      .mkString("\n") // Une todas las lineas con un salto de linea entre cada una
    s"$header\n$lines"
  }

  def formatEntityStatsHierarchy(counts: Map[String, Map[String, Int]]): String = {
    val header = "=== Estadísticas jerárquicas ==="
    val lines = counts.toList
      .filter { case (_, data) => data("Total") > 0 } // se sacan las jerarquias sin apariciones
      .sortBy { case (_, data) => -data("Total") } // ordena mayor a menor en base a "Total"
      .map { case (parent, data) =>
        val childLines = data
          .filter { case (k, _) => k != "Total" && k != "Directo" } // Saca Total y Directo para dejar solo los hijos
          .map { case (child, count) => s"  $child: $count" } 
          .mkString("\n") // Transforma los hijos en una lista de strings
        s"$parent: ${data("Total")}\n$childLines\n  (${parent} directa): ${data("Directo")}"
      }
      .mkString("\n")
    s"$header\n$lines"
  }
}
