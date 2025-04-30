const parser = require("@babel/parser");
const traverse = require("@babel/traverse").default;
const types = require("@babel/types");
const generator = require("@babel/generator").default;

module.exports = function functionifyImportLoader(source) {


  const ast = parser.parse(source, {
    sourceType: "module",
    plugins: ["jsx", "typescript"], // Include jsx if needed
  });

  const functionifiedImports = new Set();

  traverse(ast, {
    ImportDeclaration(path) {
      const importPath = path.node.source.value;

      if (
        path.node.specifiers.length === 1 &&
        types.isImportNamespaceSpecifier(path.node.specifiers[0])
      ) {
        const localName = path.node.specifiers[0].local.name;

        // Remember which identifier we have replaced
        functionifiedImports.add(localName);

        // Replace import with: let Something = function() { return require('...'); }
        const funcDecl = types.variableDeclaration('let', [
          types.variableDeclarator(
            types.identifier(localName),
            types.functionExpression(
              null,
              [],
              types.blockStatement([
                types.returnStatement(
                  types.callExpression(types.identifier('require'), [
                    types.stringLiteral(importPath)
                  ])
                )
              ])
            )
          )
        ]);

        path.replaceWith(funcDecl);
      }
    },


    Identifier(path) {
      try {
      
        if (
          functionifiedImports.has(path.node.name) && types.isMemberExpression(path.parent)) {
          if (!path.parent.object.callee) {
            path.replaceWith(types.callExpression(path.node, []));
          } else {
            if (!functionifiedImports.has(path.parent.object.callee.name)) {
              path.replaceWith(types.callExpression(path.node, []));
            }
          }
        }
      } catch (e) {
        console.log(path.parent.object.callee.name);
        console.log(path.node.name);
        console.log(e);
      }
    },
  });


  const output = generator(ast, {}, source);
  return output.code;
};