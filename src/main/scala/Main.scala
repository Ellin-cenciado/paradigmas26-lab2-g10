package Dictionary{}

import Dictionary._
// =====================================================================
// Ejercicio 6: Integración del sistema completo
// =====================================================================

object Main {
  def main(args: Array[String]): Unit = {
    // ------------------------------------------------------------------
    // Paso 1: Cargar diccionarios
    // ------------------------------------------------------------------
    val dictionary: List[NamedEntity] = Dictionary.loadAll()
    println(s"Diccionario cargado: ${dictionary.size} entidades.")

    // ------------------------------------------------------------------
    // Paso 2: Descargar posts
    // ------------------------------------------------------------------
    val subscriptions = FileIO.readSubscriptions()

    val allPosts: List[(String, List[String])] = subscriptions.map { url =>
      println(s"Descargando posts de: $url")
      val json   = FileIO.downloadFeed(url)
      val titles = FileIO.extractPostTitles(json)
      (url, titles)
    }

    // ------------------------------------------------------------------
    // Paso 3: Detectar entidades y mostrar resultados por post
    // ------------------------------------------------------------------
    // TODO (Ejercicios 3, 4 y 6):
    //   Para cada post:
    //     1. Detectar entidades
    //     2. Formatear y mostrar el resultado

    val allDetected: List[NamedEntity] = allPosts.flatMap { case (_, titles) =>
      titles.flatMap { title =>
        val found = Analyzer.detectEntities(title, dictionary)
        println(Formatters.formatNERResult(title, found))
        found
      }
    }

    // ------------------------------------------------------------------
    // Paso 4: Estadísticas globales
    // ------------------------------------------------------------------
    // TODO (Ejercicios 5 y 6):
    //   1. Recolectar TODAS las entidades detectadas en todos los posts
    //   2. Contar por tipo
    //   3. Mostrar el resumen
    val counts = Analyzer.countByType(allDetected)
    println(Formatters.formatEntityStats(counts))
  }
}
