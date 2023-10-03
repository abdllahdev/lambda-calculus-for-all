export type Bool<T> = (x: T, y: T) => T;

export const _true = <T>(t: T, _f: T) => t;
export const _false = <T>(_t: T, f: T) => f;

export const and =
  <T>(x: Bool<T>, y: Bool<T>) =>
  (t: T, f: T) =>
    x(y(t, f), f);

export const or =
  <T>(x: Bool<T>, y: Bool<T>) =>
  (t: T, f: T) =>
    x(t, y(t, f));

export const not =
  <T>(x: Bool<T>) =>
  (t: T, f: T) =>
    x(f, t);

type Op = "AND" | "OR" | "NOT";

const stringOfBooleans = (b: Bool<string>) => b("true", "false");

export const testOp = ({ op }: { op: Op }) => {
  const truthTable1 = [_true, _false];

  const truthTable2 = [
    [_false, _false],
    [_true, _false],
    [_false, _true],
    [_true, _true],
  ];

  switch (op) {
    case "AND":
      return (
        "AND Results:\n" +
        truthTable2
          .map(
            ([x, y]) =>
              `(${stringOfBooleans(x)}, ${stringOfBooleans(y)}) -> ${stringOfBooleans(and(x, y))}`,
          )
          .join("\n")
      );
    case "OR":
      return (
        "OR Results:\n" +
        truthTable2
          .map(
            ([x, y]) =>
              `(${stringOfBooleans(x)}, ${stringOfBooleans(y)}) -> ${stringOfBooleans(or(x, y))}`,
          )
          .join("\n")
      );
    case "NOT":
      return (
        "NOT Results:\n" +
        truthTable1.map((x) => `(${stringOfBooleans(x)}) -> ${stringOfBooleans(not(x))}`).join("\n")
      );
  }
};
