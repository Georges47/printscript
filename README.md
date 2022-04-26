### Run project with file path
Interpretar un archivo: ./gradlew :app:run --args="filepath=<FilePath>" -q --console=plain

Si se desea mostrar el AST agregar la opción printAST=true, quedando así: Interpretar un archivo: 
./gradlew :app:run --args="filepath=<FilePath> printAST=true" -q --console=plain

Si se quiere solo parsear y validar hacer:
./gradlew :app:run --args="filepath=<FilePath> onlyParseAndValidate=true" -q --console=plain

### View usage
./gradlew :app:usage o solo ./gradlew usage

### Run all tests
./gradlew test

### Format
Todo el proyecto: ./gradlew scalafmt

Un único subproyecto: ./gradlew :app:scalafmt