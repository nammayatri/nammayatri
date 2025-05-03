const parser = require("@babel/parser");
const traverse = require("@babel/traverse").default;
const types = require("@babel/types");
const generator = require("@babel/generator").default;
const fs = require("fs");
const path = require("path");
const crypto = require("crypto");


function createTempDir() {
  const tempDir = fs.mkdtempSync(path.join("./webPackLoader", "webpack-loader-"));
  return tempDir;
}


module.exports = function functionifyImportLoader(source) {
  // const tempDir = createTempDir();
  // const moduleHash = crypto.createHash("md5").update(source).digest("hex").substring(0, 8);
  // const modulePath = path.join(tempDir, moduleHash + ".js");


  const ast = parser.parse(source, {
    sourceType: "module",
    plugins: ["jsx", "typescript"], // Include jsx if needed
  });

  const functionifiedImports = new Set();

  traverse(ast, {
    // Program: {
    //   enter(path) {
    //     const lazyRequireFunction = types.functionDeclaration(
    //       types.identifier("lazyRequire"),
    //       [types.identifier("path")],
    //       types.blockStatement([
    //         types.variableDeclaration("let", [
    //           types.variableDeclarator(types.identifier("_cache"), types.nullLiteral()),
    //         ]),
    //         types.returnStatement(
    //           types.callExpression(types.arrowFunctionExpression([], types.blockStatement([
    //             types.ifStatement(
    //               types.unaryExpression("!", types.identifier("_cache")),
    //               types.blockStatement([
    //                 types.expressionStatement(
    //                   types.assignmentExpression(
    //                     "=",
    //                     types.identifier("_cache"),
    //                     types.callExpression(types.identifier("require"), [types.identifier("path")])
    //                   )
    //                 ),
    //               ])
    //             ),
    //             types.returnStatement(types.identifier("_cache"))
    //           ])), [])
    //         ),
    //       ])
    //     );
    //     path.unshiftContainer("body", lazyRequireFunction);
    //   },
    // },

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
  // fs.writeFileSync(modulePath, output.code);



  return output.code;
};