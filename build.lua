var "builddir" ".build"

rule "ghc" {
    command = {
        "ghcup run stack ghc -- --",
        "-O3",
        "-outputdir $builddir",
        "$in -o $out",
    },
}

local doc = pipe {
    rule "ypp.md" { command = "ypp $in -o $out" },
    rule "pandoc" { command = "pandoc -f markdown+lhs -t gfm $in -o $out" },
}

ls "*.lhs"
: foreach(function(lhs)
    doc(lhs:splitext()..".md") { lhs,
        implicit_in = build("$builddir"/lhs:splitext()) { "ghc", lhs },
    }
end)
