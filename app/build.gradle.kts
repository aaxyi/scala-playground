plugins { 
    scala
    application
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.scala-lang:scala3-library_3:3.2.1-RC2")
}

application {
    mainClass.set("org.hexalite.foundation.App")
}
