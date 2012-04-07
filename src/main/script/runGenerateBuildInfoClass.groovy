def generatedSrcBaseDirectory = "${pom.basedir}/target/generated-sources/src/main/java"
def generatedClassSrcPackage = "org.littlegrid"
def generatedSrcClassDirectory = generatedSrcBaseDirectory + "/" + generatedClassSrcPackage.replaceAll("\\.", "/")
def name = "littlegrid"
def className = "Info"
def versionNumber = pom.version

def buildDate = new Date()
def classSource = """package $generatedClassSrcPackage;

/**
 * littlegrid build information.
 */
public class $className {
    public static final String NAME = "$name";
    public static final String VERSION_NUMBER = "$versionNumber";
    public static final String BUILD_DATE = "$buildDate";


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
