package com.rtjvm.scala.oop.commands


class Rm (name: String) extends Command {
  override def apply(state : State) : State = {
    val wd = state.wd

    val absolutePath =
      if(name.startsWith(Directory.SEPERATOR)) name
      else wd.path + Directory.SEPERATOR + name

    if(Directory.ROOT_PATH.equals(absolutePath))
      state.setMessage("Nuclear War not supported yet")
    else
      doRm(state, absolutePath)
  }

  def doRm(state : State, path : String) : State = {
    def rmHelper(currentDirectory : Directory, path : List[String]) : Directory = {
      if (path.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.removeEntry(path.head)
    }
    val tokens = path.substring(1).split(Directory.SEPERATOR).toList
    val newRoot : Directory = rmHelper(state.root, path)

    if(newRoot == state.root)
      state.setMessage(path + ": no such file or directory")
    else
      State(newRoot, newRoot.findDescendent(state.wd.path.substring(1)))
  }
}