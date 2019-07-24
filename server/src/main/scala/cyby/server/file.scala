/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server

import cats.implicits._

import doobie._
import doobie.implicits._

/**
  * Utility functions for loading files from a relational
  * database and hashing and storing them in such a database.
  *
  * As a default, CyBy stores linked files in a relational database. In order
  * to save space, file blobs are stored in a separate table together with
  * a sha256 hash of the content. Another table holds links from
  * paths of data objects (type Path) to the file content. Whenver a new file
  * is linked, its hash is compared against the other hashes already stored
  * and, in case of a match, file contents are compared byte by byte. If two
  * files are identical, the second file is not added to the database and
  * the link points to the content already present in the database.
  */
trait FileEnv extends ServerEnv {

  /**
    * returns the file (blob) linked to the given path object.
    */
  def get(pth: Path): IO[Option[Array[Byte]]] =
    sql"""select files.content from files inner join filelink on files.id=filelink.fileid where filelink.pth = ${pth.toString}"""
      .query[Array[Byte]].option.transact(transactor)


  /**
    * tries to insert a file (blob) linked to the object of the
    * given path. The file is first hashed and the hash is compared
    * against hashes of other files. In case of an overlap, the contents
    * of the two files are compared byte by byte. Only if the file
    * has not yet been inserted, is a new entry created. A link from the
    * given path to the given file is created anyway, however.
    */
  def insert(pth: Path)(blob: Array[Byte]): IO[Unit] = {
    val hash = sha256(blob)

    val run =
      for {
        o <- sql"""select id, content from files where hash = $hash"""
               .query[(Long,Array[Byte])].option
        i <- o.collect{ case (i,b) if b.sameElements(blob) ⇒ i} 
              .fold(
                sql"insert into files (hash, content) values ($hash, $blob)"
                  .update.run *> sql"select last_insert_rowid()".query[Long].unique
              ){ i ⇒ i.pure[ConnectionIO] }
        _ <- sql"insert into filelink (pth, fileid) values (${pth.toString}, $i)".update.run
      } yield ()

    run transact transactor
  }

  /**
    * deletes a link from the given path. the linked file is not removed,
    * however.
    */
  def delete(pth: Path): IO[Unit] =
    sql"""delete from filelink where pth = ${pth.toString}"""
      .update.run.transact(transactor).void

  private def sha256(blob: Array[Byte]): Array[Byte] =
    java.security.MessageDigest.getInstance("SHA-256").digest(blob)
}
