def generatedSrcBaseDirectory = "${pom.basedir}/target/generated-sources/src/main/java"
def generatedClassSrcPackage = "org.littlegrid.impl"
def generatedSrcClassDirectory = generatedSrcBaseDirectory + "/" + generatedClassSrcPackage.replaceAll("\\.", "/")
def name = "littlegrid"
def className = "Info"
def versionNumber = pom.version

def buildDate = new Date()
def classSource = """package $generatedClassSrcPackage;

/**
 * littlegrid build information.
 */
final class $className {
    private static final String NAME = "$name";
    private static final String VERSION_NUMBER = "$versionNumber";
    private static final String BUILD_DATE = "$buildDate";


    private $className() {
    }

    public static String getName() {
        return NAME;
    }

    public static String getVersionNumber() {
        return VERSION_NUMBER;
    }

    public static String getBuildDate() {
        return BUILD_DATE;
    }
}
"""

println "Class $className will be written to: $generatedSrcClassDirectory"
new File(generatedSrcClassDirectory).mkdirs()

def file = new File("$generatedSrcClassDirectory/${className}.java")
file.write(classSource)
