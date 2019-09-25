/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

package cyby
package server
package example

import cats.implicits._

import cyby.dat._, example.{DataType ⇒ DT, _}
import cyby.query.{ReadPred ⇒ RP, _}

import org.http4s.dsl.io.{ Root ⇒ _, NotFound ⇒ _, Found ⇒ _, _ }
 
/**
  * Component used to process combined queries
  */
case class Query(coreSettings: CoreSettings) extends CyByZ {
  val M = CyByMonadIO.authEnv[Unit]
  val field = Field(coreSettings)

  object MP extends CyByMonad[Pure,LoggedInEnv,Unit]

  def prog(r: Request): M.Prog[Result] = r match {
    case POST -> _/_/DT(CpdT) ⇒ runCpds(r)
    case GET  -> _/_/DT(ProT) ⇒ run(ProjectS)(_.pros)
    case GET  -> _/_/DT(StoT) ⇒ run(LocationS)(_.stos)
    case GET  -> _/_/DT(SupT) ⇒ run(SupplierS)(_.sups)
    case GET  -> _/_/DT(MetT) ⇒ run(MethodS)(_.mets)
    case GET  -> _/_/DT(UseT) ⇒ run(UserS)(_.uses)
    case r                    ⇒ M raise NotFound(r.uri.renderString)
  }

  def run(e: DBEditor)(get: St ⇒ Map[e.Id,e.Srv]): M.Prog[Result] =
    M.ask >>= { le ⇒ M wrapValidated e.getAll(le.authSt, get(le.st)) }

  def runCpds(r: Request): M.Prog[Result] = for {
    le     <- M.ask
    q      <- M.decodeReq[ZQuery](r)
    res    <- q.qtype match {
                case SubstanceQuery  ⇒ subQuery(q, le).trans(Pure.to[IO])
                case StatisticsQuery ⇒ statsQuery(q, le).trans(Pure.to[IO])
              }
  } yield res

  def queryCpdsUnsorted(q: ZQ, le: LoggedInEnv): MP.Prog[List[Compound.Cli]] = for {
      filter <- MP wrapValidated readFilter(q, le.st)
      db     =  filter(CompoundS.accDB(le.authEnv, le.st.subs))
      _      <- MP.debug(s"Mapped result: Size ${db.size}")
      r      <- MP wrapValidated CompoundS.dbasmbl.run(le.st, db)
      _      <- MP.debug(s"Assembled result: Size ${r.size}")
    } yield r

  def queryCpds(q: ZQuery, le: LoggedInEnv): MP.Prog[List[Compound.Cli]] =
    queryCpdsUnsorted(q.query, le).map{ r ⇒ 
      val sort = getSort(q)

      if (q.reverse) sort(r).reverse else sort(r)
    }

  def subQuery(q: ZQuery, le: LoggedInEnv): MP.Prog[Result] =
    queryCpds(q, le) map (ss ⇒ 
      CpdRes(Found(ss drop q.start take q.count, ss.size, q.start))
    )

  def statsQuery(q: ZQuery, le: LoggedInEnv): MP.Prog[Result] =
    queryCpdsUnsorted(q.query, le).map(Compound.toStats).map{ r ⇒ 
      val sort = getStatsSort(q)

      val sorted  = if (q.reverse) sort(r).reverse else sort(r)
      BioStatsRes(Found(sorted drop q.start take q.count, r.size, q.start))
    }

  def getSort(q: ZQuery): List[Compound.Cli] ⇒ List[Compound.Cli] = q.sort match {
    case ExportCpd(f) ⇒ field.sub(f).sort
    case _            ⇒ _ sortBy (_.id)
  }

  def getStatsSort(q: ZQuery): List[BioStats] ⇒ List[BioStats] = q.sort match {
    case ExportCpd(f)      ⇒ field.sub_[BioStats](f, _.sub).sort
    case ExportCon(f)      ⇒ field.con_[BioStats](f, _.con).sort
    case ExportStats(m,st) ⇒ _ sortBy (_.stats get m map st.get)
    case _                 ⇒ _ sortBy (_.sub.id)
  }

  private def statsPred(p: Method.Id, tpe: StatsType): RP[ContainerS.Acc] = {
    def stats(c: ContainerS.Acc): Map[Method.Id,Stats] =
       c.bio.toList.map(_._2).groupBy(_.method.v).flatMap(mp)

    def mp(p: (Method.Id,List[BiodataEntryS.Acc])): Map[Method.Id,Stats] = p match {
      case (mid,Nil)    ⇒ Map.empty
      case (mid,h::t)   ⇒ Map(mid -> Stats(Nel(h,t).map(_.value.v)))
    }

    RP.doubleO(stats(_) get p map tpe.get)
  }

  def expFilter(f: ExportField, st: St): CompoundS.MF = {
    def con(t: ContainerS.MF): CompoundS.MF = (b,s) ⇒ lensed(t(b,s))(CompoundS.L.containers)
    def bio(t: BiodataEntryS.MF): CompoundS.MF = con((b,s) ⇒ lensed(t(b,s))(ContainerS.L.bio))
    def bioFil(t: BioFilS.MF): CompoundS.MF = bio((b,s) ⇒ lensed(t(b,s))(BiodataEntryS.L.files))
    def subFil(t: CpdFilS.MF): CompoundS.MF = (b,s) ⇒ lensed(t(b,s))(CompoundS.L.files)
    def conFil(t: ConFilS.MF): CompoundS.MF = con((b,s) ⇒ lensed(t(b,s))(ContainerS.L.files))

    f match {
      case ExportCpd(CpdFil(ff)) ⇒ subFil(toMapFilter(field fil ff que st))
      case ExportCon(ConFil(ff)) ⇒ conFil(toMapFilter(field fil ff que st))
      case ExportBio(BioFil(ff)) ⇒ bioFil(toMapFilter(field fil ff que st))
      case ExportCpd(sf)         ⇒ toMapFilter(field sub sf que st)
      case ExportCon(cf)         ⇒ con(toMapFilter(field con cf que st))
      case ExportBio(bf)         ⇒ bio(toMapFilter(field bio bf que st))
      case s@ExportStats(_,st)   ⇒ con(toMapFilter(statsPred(s.mid, st)))
    }
  }

  lazy val join: (Map[Compound.Id,CompoundS.Acc],Map[Compound.Id,CompoundS.Acc]) ⇒ Map[Compound.Id,CompoundS.Acc] = {
    val jf = joinMaps[Fil.Id,CpdFilS.Acc]((f,_) ⇒ f)
    val jb = joinMaps[BiodataEntry.Id,BiodataEntryS.Acc]((a,b) ⇒ a.copy(files = jf(a.files,b.files)))
    val jc = joinMaps[Container.Id,ContainerS.Acc]((a,b) ⇒ a.copy(files = jf(a.files,b.files), bio = jb(a.bio,b.bio)))

    joinMaps[Compound.Id,CompoundS.Acc]((a,b) ⇒ a.copy(files = jf(a.files,b.files), containers = jc(a.containers,b.containers)))
  }

  def readFilter(q: Q[ExportField], st: St): DataV[MapFilter[Compound.Id,CompoundS.Acc]] =
    mapErr(Q.mapper(q)((f,neg,s) ⇒ expFilter(f, st)(neg, s))(join))(DataErr.queryErr)
}
