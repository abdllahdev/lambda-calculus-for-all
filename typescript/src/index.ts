import * as Boolean from "./lib/booleans";

function main() {
  const and_results = Boolean.testOp({ op: "AND" });
  const or_results = Boolean.testOp({ op: "OR" });
  const not_results = Boolean.testOp({ op: "NOT" });
  console.log(`${and_results}\n\n${or_results}\n\n${not_results}`);
}

main();
