package net.rohlandm.aoc24.adapters

import net.rohlandm.aoc24.domain.InputRepository
import sttp.client4.httpclient.HttpClientSyncBackend
import sttp.client4.{UriContext, quickRequest}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.Properties

class FileAndDownloadInputRepositoryImpl extends InputRepository:
  override def getInput(day: Int): List[String] =
    val path = s"src/main/resources/input/day$day"

    val source = if Files.exists(Paths.get(path)) then Some(getSource(path)) else downloadFile(day)
      .map(content => storeFile(content, path))
      .map(getSource)

    source match
      case Some(value) => try value.getLines().toList.filterNot(line => line.isBlank) finally value.close()
      case None => List.empty

  private def getSource(path: String): Source = Source.fromFile(path)

  private def downloadFile(day: Int): Option[String] =
    Properties.envOrNone("SESSION") map (session => quickRequest
      .get(uri"https://adventofcode.com/2024/day/$day/input")
      .cookie("session", session)
      .send(HttpClientSyncBackend()).body)

  private def storeFile(content: String, path: String): String =
    Files.write(Paths.get(path), content.getBytes(StandardCharsets.UTF_8)).toUri.getPath
