/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package dat

import cats.implicits._
import UserLevel.{Guest,CommonUser,Superuser,Admin}

import org.scalacheck.{Arbitrary, Gen, Cogen}
import org.scalacheck.Shapeless._

trait DataImplicits extends TestImplicits {
  lazy val noControlGen: Gen[String] =
    arb[String] map (_ filterNot (_.isControl))

  // Alias
  lazy val aliasCharGen: Gen[String] =
    arb[String] map (_ filterNot (Alias.validChar))

  private def al(s: String): Alias = Alias(s) getOrElse alias0
  lazy val alias0: Alias = Alias.unsafe("alias0")
  lazy val alias1: Alias = Alias.unsafe("alias1")
  lazy val alias2: Alias = Alias.unsafe("alias2")
  lazy val alias3: Alias = Alias.unsafe("alias3")

  lazy val randomAliasGen: Gen[Alias] = aliasCharGen map al

  lazy val aliasGen: Gen[Alias] =
    mixedGen(randomAliasGen, alias0, alias1, alias2, alias3)

  implicit lazy val aliasArb: Arbitrary[Alias] = Arbitrary(aliasGen)
  implicit lazy val aliasCog: Cogen[Alias] = Cogen[String] contramap (_.v)

  // Amount

  private def amt(d: Double): Amount = Amount(d) getOrElse Amount.unsafe(1D)

  val amountGen: Gen[Amount] = Gen.frequency(
    1 -> (Gen.choose(0D,Double.MaxValue) map amt),
    9 -> (Gen.choose(0D,1000D) map amt)
  )

  implicit lazy val amountArb: Arbitrary[Amount] = Arbitrary(amountGen)
  implicit lazy val amountCog: Cogen[Amount] = Cogen[Double] contramap (_.v)



  // Cas Numbers
  private def cas(s: String): CasNr = CasNr(s) getOrElse ethanolCas

  val ethanolCas: CasNr = CasNr.unsafe("64-17-5")
  val methanolCas: CasNr = CasNr.unsafe("67-56-1")
  val waterCas: CasNr = CasNr.unsafe("7732-18-5")
  val uniqueCas: CasNr = CasNr.unsafe("57-24-9")

  val casStrGen: Gen[String] = for {
    n1 ← Gen.choose(1L, 9999999L)
    n2 ← Gen.choose(10L, 99L)
  } yield s"${n1}-${n2}-${CasNr.calc(n1, n2)}"

  def resetUniqueCas(c: CasNr) =
    if (c == uniqueCas) ethanolCas else c

  val randomCasGen: Gen[CasNr] = casStrGen map cas map resetUniqueCas

  val casGen: Gen[CasNr] =
    mixedGen(randomCasGen, ethanolCas, methanolCas, waterCas, CasNr.default)

  implicit lazy val casArb: Arbitrary[CasNr] = Arbitrary(casGen)
  implicit lazy val casCog: Cogen[CasNr] = Cogen[String] contramap (_.v)



  // Concentration
  private def conc(d: Double): Concentration =
    Concentration(d) getOrElse Concentration.unsafe(1D)

  val concentrationGen: Gen[Concentration] = Gen.frequency(
    1 -> Gen.choose(0D,1000D).map(conc),
    9 -> Gen.choose(0D,1D).map(conc)
  )

  implicit lazy val concArb: Arbitrary[Concentration] =
    Arbitrary(concentrationGen)
  implicit lazy val concCog: Cogen[Concentration] = Cogen[Double] contramap (_.v)


    
  // Density
  private def dens(d: Double): Density = Density(d) getOrElse Density.unsafe(1D)
  val densityGen: Gen[Density] = Gen.frequency(
    1 -> Gen.choose(0D,50D).map(dens),
    9 -> Gen.choose(0D,1.5D).map(dens),
  )

  implicit lazy val densityArb: Arbitrary[Density] = Arbitrary(densityGen)
  implicit lazy val densityCog: Cogen[Density] = Cogen[Double] contramap (_.v)

  // File Names
  private def fil(s: String): FileName = FileName(s) getOrElse fileName1

  val fileName1: FileName = FileName.unsafe("file1.pdf")
  val fileName2: FileName = FileName.unsafe("file2.pdf")
  val fileName3: FileName = FileName.unsafe("file3.pdf")

  val fileNameChars: List[Char] = FileName.validChar.toList

  val fileNameStrGen: Gen[String] = for {
    len   <- Gen.choose(0, FileName.MaxLength)
    chars <- Gen.listOfN(len, Gen oneOf fileNameChars)
  } yield chars mkString ""

  val randomFileNameGen: Gen[FileName] = fileNameStrGen map fil

  val fileNameGen: Gen[FileName] =
    mixedGen(randomFileNameGen, fileName1, fileName2, fileName3)

  implicit lazy val fileNameArb: Arbitrary[FileName] = Arbitrary(fileNameGen)
  implicit lazy val fileNameCog: Cogen[FileName] = Cogen[String] contramap (_.v)



  // Names
  private def nm(s: String): Name = Name(s) getOrElse name0
  lazy val name0: Name = Name.unsafe("name 0")
  lazy val name1: Name = Name.unsafe("name 1")
  lazy val name2: Name = Name.unsafe("name 2")
  lazy val name3: Name = Name.unsafe("name 3")
  lazy val uniqueName: Name = Name.unsafe("uniqueName")

  def resetUnique(n: Name) = if (n === uniqueName) name0 else n

  lazy val randomNameGen: Gen[Name] = noControlGen map nm map resetUnique

  lazy val nameGen: Gen[Name] =
    mixedGen(randomNameGen, name0, name1, name2, name3)

  implicit lazy val nameArb: Arbitrary[Name] = Arbitrary(nameGen)
  implicit lazy val nameCog: Cogen[Name] = Cogen[String] contramap (_.v)



  // Passwords
  private def pw(s: String): Password = Password(s) getOrElse pwGuest
  val pwGuest: Password = Password.unsafe("guest")
  val pwCommon: Password = Password.unsafe("common")
  val pwSuper: Password = Password.unsafe("super")
  val pwAdmin: Password = Password.unsafe("admin")

  val passwordRandomGen: Gen[Password] = noControlGen map pw

  lazy val passwordGen: Gen[Password] =
    mixedGen(passwordRandomGen, pwGuest, pwCommon, pwSuper, pwAdmin)

  implicit val passwordArb: Arbitrary[Password] = Arbitrary(passwordGen)

  implicit lazy val passwordCogen: Cogen[Password] =
    Cogen[String].contramap(_.v)



  // Percent
  private def perc(d: Double): Percent = Percent(d) getOrElse Percent.unsafe(10D)

  val percentGen: Gen[Percent] = Gen.choose(0D,100D) map perc

  implicit lazy val percentArb: Arbitrary[Percent] = Arbitrary(percentGen)
  implicit lazy val percentCog: Cogen[Percent] = Cogen[Double] contramap (_.v)



  // Plain Strings
  private def pl(s: String): Plain = Plain(s) getOrElse ps1
  val ps1: Plain = Plain.unsafe("plain string 1")
  val ps2: Plain = Plain.unsafe("plain string 2")
  val ps3: Plain = Plain.unsafe("plain string 3")
  val psEmpty: Plain = Plain.unsafe("")

  lazy val plainRandomGen: Gen[Plain] = noControlGen map pl

  lazy val plainGen: Gen[Plain] =
    mixedGen(plainRandomGen, ps1, ps2, ps3, psEmpty)

  implicit lazy val plainArb: Arbitrary[Plain] = Arbitrary(plainGen)
  implicit lazy val plainCog: Cogen[Plain] = Cogen[String] contramap (_.v)



  // UserLevels
  private def lvl(i: Int): UserLevel =
    UserLevel(i) getOrElse UserLevel.Guest

  lazy val userLevelRandomGen: Gen[UserLevel] = Gen.choose(0, 100000000) map lvl

  lazy val userLevelGen: Gen[UserLevel] =
    mixedGen(userLevelRandomGen, Guest, CommonUser, Superuser, Admin)

  implicit val userLevelArb: Arbitrary[UserLevel] = Arbitrary(userLevelGen)

  implicit lazy val userLevelCogen: Cogen[UserLevel] =
    Cogen[Int].contramap(_.v)

  // Colors
  lazy val randomByte: Gen[Int] = Gen.choose(0,255)

  lazy val colorGen: Gen[format.Color] = for {
    r <- randomByte
    g <- randomByte
    b <- randomByte
  } yield format.Color(r,g,b).get

  implicit val colorArb: Arbitrary[format.Color] = Arbitrary(colorGen)

  // TimeStamp
  val timeStampGen: Gen[TimeStamp] = Gen.choose(0L,Long.MaxValue) map TimeStamp.mk

  implicit lazy val timestampArb: Arbitrary[TimeStamp] = Arbitrary(timeStampGen)
  implicit lazy val timestampCog: Cogen[TimeStamp] = Cogen[Long] contramap (_.v)

  // Date
  val dateGen: Gen[Date] = Gen.choose(0L,Long.MaxValue) map Date.mk

  implicit lazy val dateArb: Arbitrary[Date] = Arbitrary(dateGen)
  implicit lazy val dateCog: Cogen[Date] = Cogen[Long] contramap (_.v)

  implicit lazy val molFiledArb: Arbitrary[MolFile] =
    Arbitrary(Gen.oneOf("CCO", "ClCCC=CCO", "CCC") map MolFile.fromString)

  implicit lazy val svgArb: Arbitrary[Svg] =
    Arbitrary(arb[Plain] map (p ⇒ Svg fromString p.v))

  // MolField
  implicit lazy val molFieldArb = shapeless.the[Arbitrary[Mol.Field]]

  // MolField
  implicit lazy val editInfoFieldArb = shapeless.the[Arbitrary[EditInfo.Field]]

  // StatsType
  implicit lazy val statsTypeArb = shapeless.the[Arbitrary[StatsType]]

  implicit lazy val editInfoArb = org.scalacheck.derive.MkArbitrary[EditInfo].arbitrary
}

// vim: set ts=2 sw=2 et:
