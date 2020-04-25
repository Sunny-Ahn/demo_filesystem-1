package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.filesystem.State

class Cd (dir: String) extends Command {
  override def apply(state: State): State = {
    // 1. find root
    val root = state.root
    val wd = state.wd
    // 2. find absolute path of directory
    val absolutePath =
      if (dir.startWith(Directory.SEPERATOR)) dir
      else if (wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPERATOR + dir

    // 3. find the directory

    // 4. change the state
  }
}