libraryDependencies += "io.spray" %%  "spray-json" % "1.3.3"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

TaskKey[Unit]("bcode"):= {
	val bcode = baseDirectory.value  / s"bcode-${scalaBinaryVersion.value}"
	bcode.mkdirs()

	val output = classDirectory.in(Compile).value.toPath
		val sources = compile.in(Compile).value.readStamps().getAllProductStamps.keySet()
	import collection.JavaConverters._
	import scala.sys.process._
	val classes = sources.asScala
		.map(classFile => output.relativize(classFile.toPath))
  	.map(relativePath => relativePath.toString().replace('/', '.').replace(".class", ""))
	println(output)
	//println(classes)
	classes.foreach { className =>
		s"javap -cp $output -p -c $className" #> bcode./(s"$className.txt") !

		s"javap -cp $output -p -c -s -v -l $className" #> bcode./(s"$className-full.txt") !

		s"javap -cp $output -p -constants $className" #> bcode./(s"$className-sigs.txt") !
	}

}


enablePlugins(JmhPlugin)

Charts.settings