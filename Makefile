BUILD = .build

LHSs := $(wildcard *.lhs)
BINs := $(patsubst %.lhs,$(BUILD)/%,$(LHSs))
MDs := $(patsubst %.lhs,%.md,$(LHSs))

all: $(MDs)

clean:
	rm -rf $(BUILD)

$(BUILD)/%: %.lhs
	mkdir -p $(BUILD)
	ghcup run stack ghc -- -- -O3 -outputdir $(BUILD) $< -o $@

%.md: %.lhs $(BUILD)/%
	ypp $< -o $(BUILD)/$(@:.md=.lhs)
	pandoc -f markdown+lhs -t gfm $(BUILD)/$(@:.md=.lhs) -o $@
