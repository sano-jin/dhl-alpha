import subprocess
import re
import sys

def extract_result(line):
    word = list(map(lambda s : re.sub('.*/', '', re.sub('\.cmx$', '', s)), line.split()))
    if len(word) == 2:
        return word[0] + ";"
    else:
        return ", ".join(word[2:]) + " -> " + word[0] +";"
    
ret = subprocess.run("ocamldep -one-line -native -I " + sys.argv[1] + " " + sys.argv[1] + "/*", shell=True, capture_output=True)
lines = ret.stdout.decode("utf-8").splitlines()
lines = list(map(extract_result, lines))
result = "digraph G {\n" + "\n".join(lines) + "\n}"
print(result)


# python3 ../scripts/dep_graph.py ../src | dot -Tsvg > output.svg
