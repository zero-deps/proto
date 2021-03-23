package proto

import com.google.protobuf.CodedOutputStream
import scala.reflect.macros.blackbox.Context

trait BuildCodec extends Common {
  val c: Context
  import c.universe._
  import definitions._

  def sizeBasic(field: FieldInfo, sizeAcc: TermName): Option[List[c.Tree]] =
    common.sizeFun(field).map(fun => List(
      q"${sizeAcc} = ${sizeAcc} + ${CodedOutputStream.computeTagSize(field.num)} + ${fun}"
    ))

  def sizeOption(field: FieldInfo, sizeAcc: TermName): Option[List[c.Tree]] =
    if (isOption(field.tpe)) {
      val tpe1 = field.tpe.typeArgs(0)
      val field1 = field.copy(getter=q"${field.getter}.get", tpe=tpe1, num=field.num) 
      common.sizeFun(field1).map(v => List(
        q"if (${field.getter}.isDefined) ${sizeAcc} = ${sizeAcc} + ${CodedOutputStream.computeTagSize(field.num)} + ${v}"
      )).orElse(Some(List(
        q"""val ${field.prepareName}: ${OptionClass}[${PrepareType}] = if (${field.getter}.isDefined) {
          val p: ${PrepareType} = implicitly[${messageCodecFor(tpe1)}].prepare(${field.getter}.get)
          ${sizeAcc} = ${sizeAcc} + ${CodedOutputStream.computeTagSize(field.num)} + ${CodedOutputStreamType.typeSymbol.companion}.computeUInt32SizeNoTag(p.size) + p.size
          Some(p)
        } else {
          None
        }"""
      )))
    } else {
      None
    }

  def sizeCollection(field: FieldInfo, sizeAcc: TermName): Option[List[c.Tree]] =
    if (isIterable(field.tpe)) {
      val value = TermName("value")
      val tpe1 = field.tpe.baseType(ItetableType.typeSymbol).typeArgs(0)
      val field1 = field.copy(getter=q"${value}", tpe=tpe1, num=field.num) 
      common.sizeFun(field1).map{ v =>
        if ( field1.tpe =:= StringClass.selfType
          || field1.tpe =:= ArrayByteType
          || field1.tpe =:= ArraySeqByteType
          || field1.tpe =:= BytesType
           ) {
          val tagSizeName = TermName(s"${field.sizeName}TagSize")
          List(
            q"val ${tagSizeName} = ${CodedOutputStream.computeTagSize(field.num)}"
          , q"var ${field.sizeName}: Int = 0"
          , q"${field.getter}.foreach((${value}: ${tpe1}) => ${field.sizeName} = ${field.sizeName} + ${v} + ${tagSizeName})"
          , q"${sizeAcc} = ${sizeAcc} + ${field.sizeName}"
          )
        } else {
          List(
            q"var ${field.sizeName}: Int = 0"
          , q"${field.getter}.foreach((${value}: ${tpe1}) => ${field.sizeName} = ${field.sizeName} + ${v})"
          , q"${sizeAcc} = ${sizeAcc} + ${CodedOutputStream.computeTagSize(field.num)} + ${CodedOutputStreamType.typeSymbol.companion}.computeUInt32SizeNoTag(${field.sizeName}) + ${field.sizeName}"
          )
        }
      }.orElse{
        val counter = TermName(c.freshName("n"))
        Some(List(
          q"val ${field.prepareName}: ${ArrayClass}[${PrepareType}] = new ${ArrayClass}[${PrepareType}](${field.getter}.size)"
        , q"var ${counter} = 0"
        , q"""${field.getter}.foreach((${value}: ${tpe1}) => {
            val p: ${PrepareType} = implicitly[${messageCodecFor(tpe1)}].prepare(${value})
            ${field.prepareName}(${counter}) = p
            ${sizeAcc} = ${sizeAcc} + ${CodedOutputStream.computeTagSize(field.num)} + ${CodedOutputStreamType.typeSymbol.companion}.computeUInt32SizeNoTag(p.size) + p.size
            ${counter} = ${counter} + 1
          })"""
        ))
      }
    } else {
      None
    }

  def sizeMessage(field: FieldInfo, sizeAcc: TermName): List[c.Tree] =
    List(
      q"val ${field.prepareName} = implicitly[${messageCodecFor(field.tpe)}].prepare(${field.getter})"
    , q"${sizeAcc} = ${sizeAcc} + ${CodedOutputStream.computeTagSize(field.num)} + ${CodedOutputStreamType.typeSymbol.companion}.computeUInt32SizeNoTag(${field.prepareName}.size) + ${field.prepareName}.size"
    )

  def size(params: List[FieldInfo], sizeAcc: TermName): List[c.Tree] = 
    params.map{field =>
      sizeBasic(field, sizeAcc)
        .orElse(sizeOption(field, sizeAcc))
        .orElse(sizeCollection(field, sizeAcc))
        .getOrElse(sizeMessage(field, sizeAcc))
    }.flatten

  def writeBasic(field: FieldInfo, os: TermName): Option[List[c.Tree]] =
    common.writeFun(field, os).map(fun => List(
      q"${os}.writeUInt32NoTag(${common.tag(field)})"
    , q"${fun}"
    ))

  def writeOption(field: FieldInfo, os: TermName): Option[List[c.Tree]] =
    if (isOption(field.tpe)) {
      val tpe1 = field.tpe.typeArgs(0)
      val field1 = field.copy(getter=q"${field.getter}.get", tpe=tpe1, num=field.num) 
      common.writeFun(field1, os).map(v => List(
        q"""if (${field.getter}.isDefined) {
          ${os}.writeUInt32NoTag(${common.tag(field)})
          ${v}
        }"""
      )).orElse(Some(List(
        q"""if (${field.prepareName}.isDefined) {
          val p = ${field.prepareName}.get
          ${os}.writeUInt32NoTag(${common.tag(field)})
          ${os}.writeUInt32NoTag(p.size)
          p.write(${os})
        }"""
      )))
    } else {
      None
    }

  def writeCollection(field: FieldInfo, os: TermName): Option[List[c.Tree]] =
    if (isIterable(field.tpe)) {
      val value = TermName("value")
      val tpe1 = field.tpe.baseType(ItetableType.typeSymbol).typeArgs(0)
      val field1 = field.copy(getter=q"${value}", tpe=tpe1, num=field.num)
      common.writeFun(field1, os).map(v =>
        if ( field1.tpe =:= StringClass.selfType
          || field1.tpe =:= ArrayByteType
          || field1.tpe =:= ArraySeqByteType
          || field1.tpe =:= BytesType
           ) {
          List(
            q"""${field.getter}.foreach((${value}: ${tpe1}) => {
              ${os}.writeUInt32NoTag(${common.tag(field)})
              ${v}
            })"""
          )
        } else {
          List(
            q"${os}.writeUInt32NoTag(${common.tag(field)})"
          , q"${os}.writeUInt32NoTag(${field.sizeName})"
          , q"${field.getter}.foreach((${value}: ${tpe1}) => ${v})"
          )
        }
      ).orElse{
        val counter = TermName(c.freshName("n"))
        Some(List(
          q"var ${counter} = 0"
        , q"""while(${counter} < ${field.prepareName}.length) {
            val p = ${field.prepareName}(${counter})
            ${os}.writeUInt32NoTag(${common.tag(field)})
            ${os}.writeUInt32NoTag(p.size)
            p.write(${os}) 
            ${counter} = ${counter} + 1
          }"""
        ))
      }
    } else {
      None
    }

  def writeMessage(field: FieldInfo, os: TermName): List[c.Tree] =
    List(
      q"${os}.writeUInt32NoTag(${common.tag(field)})"
    , q"${os}.writeUInt32NoTag(${field.prepareName}.size)"
    , q"${field.prepareName}.write(${os})"
    )

  def write(params: List[FieldInfo], os: TermName): List[c.Tree] = 
    params.map(field =>
      writeBasic(field, os)
        .orElse(writeOption(field, os))
        .orElse(writeCollection(field, os))
        .getOrElse(writeMessage(field, os))
    ).flatten

  def prepare(params: List[FieldInfo], aType: c.Type, aName: TermName, sizeAcc: TermName, osName: TermName): List[c.Tree] = {
    val PrepareName = TypeName(s"Prepare${aType.typeSymbol.name}")
    List(
      q"""class ${PrepareName}(${aName}: ${aType}) extends ${PrepareType} {
        var ${sizeAcc}: ${IntClass} = 0
        ..${size(params, sizeAcc)}
        val size: ${IntClass} = ${sizeAcc}
        def write(${osName}: ${CodedOutputStreamType}): ${UnitClass} = {
          ..${write(params, osName)}
        }
      }"""
    , q"def prepare(${aName}: ${aType}): ${PrepareType} = new ${PrepareName}(${aName})"
    )
  }

  def initArg(field: FieldInfo): c.Tree =
    if (isOption(field.tpe)) {
      q"${field.readName}"
    } else if (isIterable(field.tpe)) {
      q"${field.readName}.result"
    } else {
      val err: String = s"missing required field `${field.name}: ${field.tpe}`"
      val orElse = field.defaultValue.getOrElse(q"throw new RuntimeException(${err})")
      q"${field.readName}.getOrElse($orElse)"
    }

  def read(params: List[FieldInfo], t: c.Type, buildResult: Option[c.Tree]=None): c.Tree = {
    val init: List[c.Tree] = 
      if (isTrait(t)) {
        List(q"var readRes: ${OptionClass}[${t}] = ${NoneModule}")
      } else {
        params.map(field =>
          if (isOption(field.tpe)) {
            q"var ${field.readName}: ${field.tpe} = ${NoneModule}"
          } else if (isIterable(field.tpe)) {
            val tpe1 = field.tpe.baseType(ItetableType.typeSymbol).typeArgs(0)
            q"var ${field.readName}: ${builder(tpe1, field.tpe)} = ${field.tpe.typeConstructor.typeSymbol.companion}.newBuilder"
          } else {
            q"var ${field.readName}: ${OptionClass}[${field.tpe}] = ${NoneModule}"
          }
        )
      }

    val result = {
      buildResult match {
        case Some(fun1) => q"${fun1}"
        case None =>
          if (isTrait(t)) {
            val err: String = s"missing one of required field ${params.map(field => field.name.toString + ": " + field.tpe).mkString("`", "` or `", "`")}"
            q"readRes.getOrElse(throw new RuntimeException(${err}))"
          } else if (t.typeSymbol.companion == NoSymbol) {
            q"${t.typeSymbol.owner.asClass.selfType.member(TermName(t.typeSymbol.name.encodedName.toString))}"
          } else {
            val args = params.map(field => q"${field.name} = ${initArg(field)}")
            q"${t.typeSymbol.companion}.apply(..${args})"  
          }
      }
    }

    def putLimit(is: TermName, read: c.Tree): List[c.Tree] = 
      List(
        q"val readSize: Int = ${is}.readRawVarint32"
      , q"val limit = ${is}.pushLimit(readSize)"
      , q"${read}"
      , q"${is}.popLimit(limit)"
      )

    def tagMatch(is: TermName): List[c.Tree] = 
      params.map{ field =>
        val readContent: List[c.Tree] = common.readFun(field, is).map(readFun => List(
          q"${field.readName} = Some(${readFun})"
        )).getOrElse{
          val value = TermName("value")
          if (isOption(field.tpe)) {
            val tpe1 = field.tpe.typeArgs(0)
            val field1 = field.copy(getter=q"${value}", tpe=tpe1, num=field.num)
            common.readFun(field1, is).map(readFun => List(
              q"${field.readName} = Some(${readFun})"
            )).getOrElse(
              putLimit(is, q"${field.readName} = Some(implicitly[${messageCodecFor(tpe1)}].read(${is}))")
            )
          } else if (isIterable(field.tpe)) {
            val tpe1 = field.tpe.baseType(ItetableType.typeSymbol).typeArgs(0)
            val field1: FieldInfo = field.copy(getter=q"${value}", tpe=tpe1, num=field.num)
            common.readFun(field1, is).map(readFun =>
              if ( field1.tpe =:= StringClass.selfType
                || field1.tpe =:= ArrayByteType
                || field1.tpe =:= ArraySeqByteType
                || field1.tpe =:= BytesType
                 ) {
                q"${field.readName} += ${readFun}" :: Nil
              } else {
                val readMessage: c.Tree = q"""while (${is}.getBytesUntilLimit > 0) {
                  ${field.readName} += ${readFun}
                }"""
                putLimit(is, readMessage)
              }
            ).getOrElse{
              val readMessage: c.Tree = q"${field.readName} += implicitly[${messageCodecFor(tpe1)}].read(${is})"
              putLimit(is, readMessage)
            }
          } else {
            putLimit(is, q"${field.readName} = Some(implicitly[${messageCodecFor(field.tpe)}].read(${is}))")
          }
        }
        cq"${common.tag(field)} => ..${readContent}"
      }

    val isName = TermName("is")

    if (params.size > 0) {
      q"""def read(${isName}: ${CodedInputStreamType}): ${t} = {
        var done = false
        ..${init}
        while(done == false) {
          ${isName}.readTag match {
            case ..${tagMatch(isName)}
            case 0 => done = true
            case skip => ${isName}.skipField(skip)
          }
        }
        ${result}
      }"""
    } else {
      q"def read(${isName}: ${CodedInputStreamType}): ${t} = ${result}"
    }
  }
}
