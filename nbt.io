// TODO: 
// - import from gzip'd / zlib'd NBTs
// - endianness + convert

NamedTag := Object clone
NamedTag clone := NamedTag
NamedTag payloads := list( // length of payload or prefix in bytes
    list("TEnd", 0),
    list("TByte", 1),
    list("TShort", 2),
    list("TInt", 4),
    list("TLong", 8),
    list("TFloat", 4),
    list("TDouble", 8),
    list("TByte_Array", 4),
    list("TString", 2),
    list("TList", 5),
    list("TCompound", 0),
    list("TInt_Array", 4)
)
NamedTag payload := method(key, NamedTag payloads detect(el, el at(0) == key) at(1))
NamedTag types := NamedTag payloads map(el, el at(0))
NamedTag fromId := method(i, _ getSlot(NamedTag types at(i)))


Tag := Object clone
Tag name ::= "<no name>"
Tag value ::= nil // num, string, list(tags), ...
Tag parent ::= nil // tag

Tag print := method(indent,
    if(indent == nil, indent = "")

    str := "#{indent}[#{self type}] #{self name}" interpolate

    if(list("TList", "TCompound") contains(self type)) then(
        "#{str}\n#{indent}{" interpolate println
        self value foreach(tag, tag print(indent .. "    "))
        "#{indent}}" interpolate println

    ) elseif(list("TByte_Array", "TInt_Array") contains(self type)) then(
        "#{str} ... (#{self value size} items)" interpolate println

    ) else (
        "#{str} = #{self value}" interpolate println
    )
)

NamedTag types foreach(typ,
    setSlotWithType(typ, Tag clone)
)

TList tag_type ::= nil


// --------------------------------------


Tag convert := method(buf, buf reverse asBinaryUnsignedInteger) // STUB + WRONG because endianness / int64-support
TLong convert := method(buf, buf reverse asBinaryNumber)        // TODO: proper endianness convert
TDouble convert := method(buf, buf reverse asBinaryNumber)      //       according to tag types
TString convert := method(buf, buf asUTF8)


Tag readBytes := method(file, len,
    if(len == nil, len = NamedTag payload(self type))
    self convert( file readBufferOfLength(len) )
)

Tag setName := method(file,
    len := TShort readBytes(file)
    if(len > 0, self name = TString readBytes(file, len))
)

Tag setValue := method(file, self value = readBytes(file))

TString setValue := method(file,
    len := TShort readBytes(file)
    if(len > 0, self value = TString readBytes(file, len))
)

TByte_Array setValue := method(file,
    self value = list()
    len := TInt readBytes(file)
    len repeat(
        self value append( TByte readBytes(file) )
    )
)

TInt_Array setValue := method(file,
    self value = list()
    len := TInt readBytes(file)
    len repeat(
        self value append( TInt readBytes(file) )
    )
)

TList setValue := method(file,
    self tag_type = NamedTag fromId(TByte readBytes(file))
    self value = list()
    len := TInt readBytes(file)
    len repeat(
        tag := tag_type clone
        tag parent = self
        tag setValue(file)
        
        self value append(tag)
    )
)

TCompound setValue := method(file,
    self value = list()
    next_tag_type := NamedTag fromId(TByte readBytes(file))

    while(next_tag_type != TEnd,
        tag := next_tag_type clone

        tag parent = self
        tag setName(file)
        tag setValue(file)

        self value append(tag)
        next_tag_type = NamedTag fromId(TByte readBytes(file))
    )

    self
)

// -----------------------------------------------


fname := System args at(1)
if(fname == nil,
    "Usage: io nbt.io FILENAME" println
    System exit(1)
)

root ::= nil
file := File clone openForReading(fname)
(file readBufferOfLength(1) asBinarySignedInteger == 10) \
    ifFalse(
        "Not a nbt-file." println
        System exit(1)
    ) \
    ifTrue(
        root = TCompound clone
        root setName(file)
        root setValue(file)
    )

root print 