/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package chem

import org.scalacheck.{Gen, Arbitrary}

trait Generators extends cyby.TestImplicits {
  lazy val atomPairGen: Gen[(String,Int)] = Gen.frequency(
    10 -> ("C"  -> 4),
    2  -> ("O"  -> 2),
    2  -> ("N"  -> 3),
    1  -> ("F"  -> 1),
    1  -> ("Cl" -> 1),
    1  -> ("Br" -> 1),
  )

  lazy val atomGen: Gen[String] = atomPairGen map (_._1)

  lazy val chainGen: Gen[String] = for {
    head <- atomGen
    tail <- Gen option atomGen
    res  <- tail.fold(Gen const head)(t ⇒
              for {
                n  <- Gen.choose(0,4)
                ps <- Gen.listOfN(n, atomPairGen)
                as = ps collect { case (a,c) if c > 1 ⇒ a } mkString ""
              } yield  s"${head}${as}${t}"
            )
  } yield res

  lazy val branchGen: Gen[(String,Int)] = for {
    p     <- atomPairGen
    (a,c) = p
    n  <- Gen.choose(0, c-1)
    cs <- Gen.listOfN(n, chainGen)
    s  =  cs map (s ⇒ s"(${s})") mkString ""
  } yield (s"${a}${s}", c - n)

  lazy val smilesGen: Gen[String] = for {
    a <- chainGen
    n <- Gen.choose(1,10)
    ps <- Gen.listOfN(n, branchGen)
    bs = ps collect { case (s,c) if c >= 2 ⇒ s } mkString ""
  } yield s"${a}${bs}"

  lazy val uniqueSmiles = "CCSCC"
  lazy val uniqueMol: Mol = Mol.read(uniqueSmiles).get

  def replaceUniqueSmiles(s: String) =
    if (s == uniqueSmiles) "CCOCC" else s

  implicit lazy val molArb: Arbitrary[Mol] =
    Arbitrary(smilesGen map (s ⇒ Mol.read(replaceUniqueSmiles(s)).get))
}

// vim: set ts=2 sw=2 et:

