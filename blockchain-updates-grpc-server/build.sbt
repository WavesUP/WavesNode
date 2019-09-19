addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3")
addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1")

fs2GrpcServiceSuffix := "F"
scalapbCodeGeneratorOptions += CodeGeneratorOption.FlatPackage
includeFilter in PB.generate := protoFilter
PB.protoSources in Compile += PB.externalIncludePath.value

enablePlugins(Fs2Grpc)

name := "blockchain-updates-grpc-server"
resolvers += Resolver.sonatypeRepo("snapshots")
updateOptions := updateOptions.value.withLatestSnapshots(false)
organization := "com.wavesplatform"
organizationName := "Waves Platform"
organizationHomepage := Some(url("https://wavesplatform.com"))
version := "0.0.1"
scalaVersion := "2.12.9"
scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-Ywarn-unused:-implicits",
  "-Xlint",
  "-Ypartial-unification",
  "-opt:l:inline",
  "-opt-inline-from:**"
)

libraryDependencies ++= Seq(
  ("com.wavesplatform"   % "protobuf-schemas"      % "1.0.0-SNAPSHOT" classifier "proto" changing ()) % "protobuf",
  "io.grpc"              % "grpc-netty"            % scalapb.compiler.Version.grpcJavaVersion,
  "com.thesamet.scalapb" %% "scalapb-runtime-grpc" % scalapb.compiler.Version.scalapbVersion,
)

lazy val protoFilter = new SimpleFileFilter(
  (f: File) => f.getName.endsWith(".proto") && (f.getParent.endsWith("waves") || f.getParent.endsWith("events")))
