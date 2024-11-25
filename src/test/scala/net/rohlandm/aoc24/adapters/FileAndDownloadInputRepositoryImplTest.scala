package net.rohlandm.aoc24.adapters

import org.scalatest.funsuite.AnyFunSuiteLike

class FileAndDownloadInputRepositoryImplTest extends AnyFunSuiteLike {

  test("get Input for day 0 returns list of 3 Strings"):
    assert(FileAndDownloadInputRepositoryImpl().getInput(0).size === 3)
    assert(FileAndDownloadInputRepositoryImpl().getInput(0).head === "first Line")
    assert(FileAndDownloadInputRepositoryImpl().getInput(0).apply(1) === "second Line")
    assert(FileAndDownloadInputRepositoryImpl().getInput(0).last === "another Line")
}
