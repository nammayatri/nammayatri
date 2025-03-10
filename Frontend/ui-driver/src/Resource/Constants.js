export const extractKeyByRegex = (regex, text) => {
  const matches = text.match(regex);
  return matches ? matches[0] : "";
}
