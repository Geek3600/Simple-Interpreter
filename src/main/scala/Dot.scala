package com.liangdp.graphviz4s
import sys.process._
import java.io.File
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import com.liangdp.graphviz4s.Backend.viewFileCommand
/**
 * Assemble, save, and render DOT source code, open result in viewer.
 * @author Depeng Liang
 */

 //TODO:: 完善画图功能，支持更多的符号
abstract class Dot(
  var name: String = null, var comment: String = null,
  var strict: Boolean = false,
  var graphAttr: Map[String, String] = Map[String, String](),
  var nodeAttr: Map[String, String] = Map[String, String](),
  var edgeAttr: Map[String, String] = Map[String, String](),
  val body: ArrayBuffer[String] = ArrayBuffer[String]()) {

  protected def _head: String
  protected def _edge: String // = "\t\t%s -> %s %s"
  protected def _edgePlain: String

  protected val _comment = "// %s"
  protected val _subgraph = "subgraph %s{"
  protected val _node = "\t%s %s"
  protected val _tail = "}"

  protected val HTML_STRING =
    """</?\w+((\s+\w+(\s*=\s*(?:".*?"|'.*?'|[\^'">\s]+))?)+\s*|\s*)/?>""".r

  /**
   * Return DOT identifier from string, quote if needed.
   */
  protected def quote(identifier: String): String = {
    if (HTML_STRING.findAllMatchIn(identifier).length <= 0) {
      var result = identifier
      if (result(0) != '"') result = s""""$result"""
      if (result(result.length - 1) != '"') result = s"""$result""""
      result
    } else identifier
  }

  /**
   * Return assembled DOT attributes string.
   */
  protected def attribute(label: String = null, attrs: Map[String, String] = null): String = {
    val tmpAttr = if (attrs == null) Map[String, String]() else attrs
    if (label != null) {
      s"""[label=${quote(label)} """ +
        s"""${tmpAttr.foldLeft(""){ (acc, elem) => s"$acc ${elem._1}=${elem._2}"}}]"""
    }
    else {
      s"""[${tmpAttr.foldLeft(""){ (acc, elem) => s"$acc ${elem._1}=${elem._2}"}}]"""
    }
  }

  /**
   * Create a node.
   * @param name Unique identifier for the node inside the source.
   * @param label Caption to be displayed (defaults to the node name).
   * @param attrs Any additional node attributes (must be strings).
   */
  def node(name: String, label: String = null, attrs: Map[String, String] = null): Unit = {
    this.body += _node.format(quote(name), attribute(label, attrs))
  }

  /**
   * Create an edge between two nodes.
   * @param tailName Start node identifier.
   * @param headName End node identifier.
   * @param label Caption to be displayed near the edge.
   * @param attrs Any additional edge attributes (must be strings).
   */
  def edge(tailName: String, headName: String,
      label: String = null, attrs: Map[String, String] = null): Unit = {
    this.body += _edge.format(tailName, headName, attribute(label, attrs))
  }

  /**
   * Create a bunch of edges.
   * @param tailName Start node identifier.
   * @param headNames End nodes identifier.
   */
  def edges(tailName: String, headNames: Array[String]): Unit = {
    edges(headNames.map(h => (tailName, h)))
  }

/**
 * Create a bunch of edges.
 * @param tailHeads array of (tailName, headName) pairs.
 */
  def edges(tailHeads: Array[(String, String)]): Unit = {
    for ((t, h) <- tailHeads) {
      this.body += this._edgePlain.format(t, h)
    }
  }

  /**
   * Add a graph/node/edge attribute statement.
   * @param kw Attributes target ("graph", "node", or "edge").
   * @param attrs  Attributes to be set.
   */
  def attr(kw: String, attrs: Map[String, String] = null): Unit = {
    val list = List("graph", "node", "edge")
    require(list.contains(kw.toLowerCase),
        s"attr statement must target graph, node, or edge: $kw")
    val line = "\t%s %s".format(kw, this.attribute(null, attrs))
    this.body += line
  }

  /**
   * The DOT source code as string.
   */
  def source(subGraph: Boolean = false): String = {
    val result = ArrayBuffer[String]()
    val sj = if (subGraph) "\t" else ""

    if (this.comment != null) result += this._comment.format(this.comment)

    val head = if (subGraph) this._subgraph else this._head
    val na = if (name != null) s"$name " else ""
    if (this.strict) result += s"${sj}strict ${head.format(na)}"
    else result += s"$sj${head.format(na)}"

    val styled = {
      if (this.graphAttr.size != 0 || this.nodeAttr.size != 0 || this.edgeAttr.size != 0) true
      else false
    }
    if (this.graphAttr.size != 0) {
      result += s"$sj\t%s %s".format("graph", this.attribute(null, this.graphAttr))
    }
    if (this.nodeAttr.size != 0) {
      result += s"$sj\t%s %s".format("node", this.attribute(null, this.nodeAttr))
    }
    if (this.edgeAttr.size != 0) {
      result += s"$sj\t%s %s".format("edge", this.attribute(null, this.edgeAttr))
    }

    val indent = if (styled) "\t" else ""
    this.body.toArray.foreach(l => result += s"$sj$indent$l")

    result += s"$sj${this._tail}"
    result.toArray.mkString("\n")
  }

  /**
   * Add the current content of the given graph as subgraph.
   * @param graph  An instance of the same kind (Graph, Digraph) as the current graph.
   */
  def subGraph(graph: Dot): Unit = {
    require(this.getClass == graph.getClass, "cannot add subgraphs of different kind")
    this.body += graph.source(subGraph = true)
  }

  /**
   * Save the DOT source to file.
   * @param filename Filename for saving the source (defaults to name + ".gv")
   * @param directory Directory for source saving and rendering.
   * @return The (possibly relative) path of the saved source file.
   */
  def save(fileName: String, directory: String): String = {
    val path = s"$directory${File.separator}$fileName"
    val writer = new PrintWriter(path)
    try {
      // scalastyle:off println
      val data = this.source()
      writer.println(data)
      writer.flush()
      // scalastyle:off println
    } finally {
      writer.close()
    }
    path
  }

  /**
   * Save the source to file and render with the Graphviz engine.
   * @param engine The layout commmand used for rendering ('dot', 'neato', ...).
   * @param format The output format used for rendering ('pdf', 'png', ...).
   * @param fileName Name of the DOT source file to render.
   * @param directory Directory to save the Dot source file.
   * @param view Whether open the rendered result with the default application.
   * @param cleanup Whether delete the source file after rendering.
   * @return The (possibly relative) path of the rendered file.
   */
  def render(engine: String = "dot", format: String = "pdf", fileName: String,
    directory: String, view: Boolean = false, cleanUp: Boolean = false): String = {
    val filePath = this.save(fileName, directory)
    val rendered = Backend.render(engine, format, filePath)
    if (cleanUp) new File(filePath).delete
    if (view) _view(rendered, format)
    rendered
  }

    /**
   * Save the source to file, open the rendered result in a viewer.
   * @param engine The layout commmand used for rendering ('dot', 'neato', ...).
   * @param format The output format used for rendering ('pdf', 'png', ...).
   * @param fileName Name of the DOT source file to render.
   * @param directory Directory to save the Dot source file.
   * @param cleanup Whether delete the source file after rendering.
   * @return The (possibly relative) path of the rendered file.
   */
  def view(engine: String = "dot", format: String = "pdf", fileName: String,
    directory: String, cleanUp: Boolean = false): String = {
    this.render(engine, format, fileName, directory, view = true, cleanUp)
  }

  /**
   * Start the right viewer based on file format and platform.
   * but know only support Linux
   */
  protected def _view(filePath: String, format: String): Unit = {
    import sys.process._
    var command = viewFileCommand()
    try {
      // s"$command $filePath".! // 注释掉，不让他显示pdf，不然wsl2会报错
    } catch { case _ : Throwable =>
      val errorMsg =
        s"""no built-in viewer support for $command $format
           |on Linux platform""".stripMargin
      throw new RuntimeException(errorMsg)
    }
  }
}

/**
 * Graph source code in the DOT language.
 * @param name Graph name used in the source code.
 * @param comment Comment added to the first line of the source.
 * @param strict Rendering should merge multi-edges (default: false).
 * @param graphAttr Mapping of (attribute, value) pairs for the graph.
 * @param nodeAttr Mapping of (attribute, value) pairs set for all nodes.
 * @param edgeAttr Mapping of (attribute, value) pairs set for all edges.
 * @param body ArrayBuffer of lines to add to the graph body.
 */
class Graph(
  name: String = null, comment: String = null,
  strict: Boolean = false,
  graphAttr: Map[String, String] = Map[String, String](),
  nodeAttr: Map[String, String] = Map[String, String](),
  edgeAttr: Map[String, String] = Map[String, String](),
  body: ArrayBuffer[String] = ArrayBuffer[String]())
  extends Dot(name, comment, strict, graphAttr, nodeAttr, edgeAttr, body){

  override def _head: String = "graph %s{"
  override def _edge: String = "\t\t%s -- %s %s"
  override def _edgePlain: String = "\t\t%s -- %s"
}

/**
 * Directed graph source code in the DOT language.
 */
class Digraph(
  name: String = null, comment: String = null,
  strict: Boolean = false,
  graphAttr: Map[String, String] = Map[String, String](),
  nodeAttr: Map[String, String] = Map[String, String](),
  edgeAttr: Map[String, String] = Map[String, String](),
  body: ArrayBuffer[String] = ArrayBuffer[String]())
  extends Dot(name, comment, strict, graphAttr, nodeAttr, edgeAttr, body){

  override def _head: String = "digraph %s{"
  override def _edge: String = "\t\t%s -> %s %s"
  override def _edgePlain: String = "\t\t%s -> %s"
}
