QUERY_EXE=../../query
QUERY=query.txt

all: trace.json
	$(QUERY_EXE) -t $< -q $(QUERY) \
	--output-directory out \
	--debug

clean:
	rm -rf inputs* snap* out *.csv
