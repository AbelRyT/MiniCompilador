using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.ObjectModel;
using System.Text;
using System.Windows;

namespace MiniCompilador
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public ObservableCollection<string> LexicalTokens { get; set; } = new();
        public ObservableCollection<string> SyntaxErrors { get; set; } = new();
        public ObservableCollection<string> SemanticErrors { get; set; } = new();
        public ObservableCollection<string> SymbolTable { get; set; } = new();
        public string TranslatedCode { get; set; } = string.Empty;
        public string IntermediateCode { get; set; } = string.Empty;

        public MainWindow()
        {
            InitializeComponent();
            LexicalListView.ItemsSource = LexicalTokens;
            SyntaxListView.ItemsSource = SyntaxErrors;
            SemanticListView.ItemsSource = SemanticErrors;
            SymbolTableListView.ItemsSource = SymbolTable;
        }

        private void AnalyzeCode_Click(object sender, RoutedEventArgs e)
        {
            string code = CodeInput.Text;
            AnalyzeAndTranslate(code);
        }

        private void AnalyzeAndTranslate(string code)
        {
            LexicalTokens.Clear();
            SyntaxErrors.Clear();
            SemanticErrors.Clear();
            SymbolTable.Clear();
            TranslatedCode = string.Empty;
            IntermediateCode = string.Empty;

            SyntaxTree syntaxTree = CSharpSyntaxTree.ParseText(code);
            CompilationUnitSyntax root = syntaxTree.GetCompilationUnitRoot();

            // Análisis Léxico
            foreach (var token in root.DescendantTokens())
            {
                LexicalTokens.Add($"Token -> '{token.Text}'");
            }

            // Análisis Sintáctico
            foreach (var diagnostic in syntaxTree.GetDiagnostics())
            {
                if (diagnostic.Severity == DiagnosticSeverity.Error)
                {
                    SyntaxErrors.Add(diagnostic.GetMessage());
                }
            }

            // Análisis Semántico
            var compilation = CSharpCompilation.Create("CodeAnalysis")
                .AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location))
                .AddSyntaxTrees(syntaxTree);

            foreach (var diagnostic in compilation.GetDiagnostics())
            {
                if (diagnostic.Severity == DiagnosticSeverity.Error)
                {
                    SemanticErrors.Add(diagnostic.GetMessage());
                }
            }

            // Generar Tabla de Símbolos
            GenerateSymbolTable(root);

            // Generar Código Intermedio
            GenerateIntermediateCode(root);

            // Traducción a C++
            TranslatedCode = TranslateToCpp(root);
            TranslatedCodeOutput.Text = TranslatedCode;
        }

        private void GenerateSymbolTable(CompilationUnitSyntax root)
        {
            SymbolTable.Clear();
            // Se recorren las clases y se extrae la información de sus miembros
            foreach (var classDecl in root.DescendantNodes().OfType<ClassDeclarationSyntax>())
            {
                SymbolTable.Add($"Clase: {classDecl.Identifier.Text}");
                // Métodos de la clase
                foreach (var methodDecl in classDecl.Members.OfType<MethodDeclarationSyntax>())
                {
                    SymbolTable.Add($"   Método: {methodDecl.Identifier.Text} - Retorna: {methodDecl.ReturnType}");
                }
                // Campos de la clase
                foreach (var fieldDecl in classDecl.Members.OfType<FieldDeclarationSyntax>())
                {
                    foreach (var variable in fieldDecl.Declaration.Variables)
                    {
                        SymbolTable.Add($"   Campo: {variable.Identifier.Text} - Tipo: {fieldDecl.Declaration.Type}");
                    }
                }
            }
        }

        private void GenerateIntermediateCode(CompilationUnitSyntax root)
        {
            StringBuilder intermediate = new StringBuilder();
            intermediate.AppendLine("// Código Intermedio Generado");
            // Para cada método se genera una representación simplificada
            foreach (var method in root.DescendantNodes().OfType<MethodDeclarationSyntax>())
            {
                intermediate.AppendLine($"BeginMethod {method.Identifier.Text}");
                if (method.Body != null)
                {
                    foreach (var statement in method.Body.Statements)
                    {
                        intermediate.AppendLine($"   {statement.Kind()} : {statement.ToString().Trim()}");
                    }
                }
                else if (method.ExpressionBody != null)
                {
                    intermediate.AppendLine($"   Expression: {method.ExpressionBody.Expression.ToString().Trim()}");
                }
                intermediate.AppendLine("EndMethod");
                intermediate.AppendLine();
            }
            IntermediateCode = intermediate.ToString();
            IntermediateCodeOutput.Text = IntermediateCode;
        }

        private string TranslateToCpp(CompilationUnitSyntax root)
        {
            StringBuilder cppCode = new StringBuilder();
            cppCode.AppendLine("#include <iostream>");
            cppCode.AppendLine("using namespace std;\n");

            // Si existen namespaces, se recorren; en caso contrario, se buscan clases en la raíz
            if (root.Members.Any(m => m is NamespaceDeclarationSyntax))
            {
                foreach (var ns in root.Members.OfType<NamespaceDeclarationSyntax>())
                {
                    foreach (var classDecl in ns.Members.OfType<ClassDeclarationSyntax>())
                    {
                        cppCode.AppendLine(TranslateClassToCpp(classDecl));
                    }
                }
            }
            else
            {
                foreach (var classDecl in root.Members.OfType<ClassDeclarationSyntax>())
                {
                    cppCode.AppendLine(TranslateClassToCpp(classDecl));
                }
            }
            return cppCode.ToString();
        }

        private string TranslateClassToCpp(ClassDeclarationSyntax classDecl)
        {
            StringBuilder classCode = new StringBuilder();
            classCode.AppendLine($"class {classDecl.Identifier.Text} {{");
            classCode.AppendLine("public:");
            foreach (var member in classDecl.Members)
            {
                if (member is MethodDeclarationSyntax methodDecl)
                {
                    classCode.AppendLine(TranslateMethodToCpp(methodDecl));
                }
            }
            classCode.AppendLine("};\n");
            return classCode.ToString();
        }

        private string TranslateMethodToCpp(MethodDeclarationSyntax methodDecl)
        {
            string returnType = MapType(methodDecl.ReturnType.ToString());
            string methodName = methodDecl.Identifier.Text;
            string parameters = string.Join(", ", methodDecl.ParameterList.Parameters.Select(p =>
                MapType(p.Type.ToString()) + " " + p.Identifier.Text));

            StringBuilder methodCode = new StringBuilder();
            methodCode.AppendLine($"    {returnType} {methodName}({parameters}) {{");

            // Si el método tiene cuerpo
            if (methodDecl.Body != null)
            {
                foreach (var statement in methodDecl.Body.Statements)
                {
                    methodCode.AppendLine(TranslateStatementToCpp(statement));
                }
            }
            // Si utiliza cuerpo de expresión (=>)
            else if (methodDecl.ExpressionBody != null)
            {
                methodCode.AppendLine("        return " + TranslateExpressionToCpp(methodDecl.ExpressionBody.Expression) + ";");
            }
            methodCode.AppendLine("    }");
            return methodCode.ToString();
        }

        // Traduce sentencias (actualmente implementa 'return')
        private string TranslateStatementToCpp(StatementSyntax statement)
        {
            if (statement is ReturnStatementSyntax returnStmt)
            {
                return "        return " + TranslateExpressionToCpp(returnStmt.Expression) + ";";
            }
            // Otros casos se pueden agregar aquí
            return "        // [Sentencia no traducida]";
        }

        // Traduce expresiones (básico: expresiones binarias, literales e identificadores)
        private string TranslateExpressionToCpp(ExpressionSyntax expr)
        {
            if (expr is BinaryExpressionSyntax binaryExpr)
            {
                string left = TranslateExpressionToCpp(binaryExpr.Left);
                string right = TranslateExpressionToCpp(binaryExpr.Right);
                string op = binaryExpr.OperatorToken.Text;
                return $"{left} {op} {right}";
            }
            else if (expr is LiteralExpressionSyntax literal)
            {
                return literal.Token.Text;
            }
            else if (expr is IdentifierNameSyntax identifier)
            {
                return identifier.Identifier.Text;
            }
            // Caso por defecto
            return expr.ToString();
        }

        // Mapeo básico de tipos
        private string MapType(string csType)
        {
            return csType switch
            {
                "int" => "int",
                "float" => "float",
                "double" => "double",
                "bool" => "bool",
                "string" => "std::string",
                "void" => "void",
                _ => csType,
            };
        }
    }
}
