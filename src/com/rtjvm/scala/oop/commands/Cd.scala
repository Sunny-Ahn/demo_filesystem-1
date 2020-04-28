package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.filesystem.State
import com.rtjvm.scala.oop.files.Directory
import com.rtjvm.scala.oop.files.DirEntry

class Cd (dir: String) extends Command {
  override def apply(state: State): State = {
    // 1. find root
    val root = state.root
    val wd = state.wd
    // 2. find absolute path of directory
    val absolutePath =
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if (wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir

    // 3. find the directory
    val destinationDirectory = doFindEntry(root, absolutePath)

    // 4. change the state
    if (destinationDirectory == null || !destinationDirectory.isDirectory)
      state.setMessage(dir + ": no such directory")
    else
      State(root, destinationDirectory.asDirectory)
  }

  def doFindEntry(root : Directory, path : String) : DirEntry = {
    @scala.annotation.tailrec
    def findEntryHelper(currentDirectory : Directory, path : List[String]) : DirEntry =
      if (path.isEmpty || path.head.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.findEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if(nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, path.tail)
      }
    @scala.annotation.tailrec
    def collapseRelativeTokens(path:List[String], result : List[String]) : List[String] =
      if (path.isEmpty) result
      else if (".".equals(path.head)) collapseRelativeTokens(path.tail, result)
      else if ("..".equals(path.head)){
        if(result.isEmpty) null
        else collapseRelativeTokens(path.tail, result.init)
      } else collapseRelativeTokens(path.tail, result :+ path.head)
    val tokens : List[String] = path.substring(1).split(Directory.SEPARATOR).toList
    val newTokens = collapseRelativeTokens(tokens, List())
    if (newTokens == null) null
    else findEntryHelper(root, newTokens)
  }
}