module GenBankFeatureTables

import Automa
import Automa.RegExp: @re_str
const re = Automa.RegExp

machine = let
    table_header = re">[^\r\n]*"
    start = re"<?[0-9]+"
    stop =  re">?[0-9]+"
    
    # It turns out that the format is completely unresolvable if fields can
    # consist of empty spaces. Hence, we define a field as being stripped of space,
    # and a spacedfield to be a field with optional whitespace sorrounding it.
    field = (re.any() \ re.space()) * re"[^\r\n\f]*"
    spacedfield = re"[ \v]*" * field

    hspace = re"[ \v\t]+"

    # We have tab-hspace because the fields may end in hspace, so we can distinguish
    # end field, then hspace from merely a field ending in hspace
    tab_hspace = re"\t" * hspace
    linefeed = re"\n"
    newline = re.opt(re"\r") * linefeed

    start_then_stop = start * re"\t" * stop
    feature_header = start_then_stop * re"\t" * spacedfield * re.opt(hspace)
    additional_range = start_then_stop * re.opt(hspace)
    qualifier = re"\t\t\t" * spacedfield * re"\t" * spacedfield * re.opt(hspace)

    feature = (
        feature_header *
        re.rep(newline * additional_range) *
        re.rep(newline * qualifier)
    )

    # Need not end with newline, but must be separated by newlines
    features = feature * re.rep(newline * feature)

    # TODO: Add trailing whitespace
    table = table_header * newline * re.opt(features)

    # Actions
    start.actions[:enter] = [:mark]
    start.actions[:exit]  = [:start_or_stop]

    stop.actions[:enter]  = [:mark]
    stop.actions[:exit]   = [:start_or_stop]

    field.actions[:enter] = [:mark]
    field.actions[:exit] = [:field]

    start_then_stop.actions[:exit] = [:start_then_stop]

    feature_header.actions[:exit] = [:feature_header]
    additional_range.actions[:exit] = [:additional_range]
    qualifier.actions[:exit] = [:qualifier]

    nfa = Automa.remove_dead_nodes(Automa.re2nfa(table))
    nfa_dot = Automa.nfa2dot(nfa)
    open(i -> write(i, nfa_dot), "/tmp/nfa.dot", "w")
    run(pipeline(`/usr/local/bin/dot -Tsvg /tmp/nfa.dot`, stdout="/tmp/nfa.svg"))
    Automa.compile(table)
end

# TODO: Use better context
context = Automa.CodeGenContext();

actions = Dict(
    :mark => quote nothing end,
    :start_or_stop => quote nothing end,
    :field => quote nothing end,
    :start_then_stop => quote nothing end,
    :feature_header => quote nothing end,
    :additional_range => quote nothing end,
    :qualifier => quote nothing end,
)

@eval function parse_genbank_header(data::Union{String, SubString{String}, Vector{UInt8}})
    $(Automa.generate_init_code(context, machine))
    
    # p_end and p_eof were set to 0 and -1 in the init code,
    # we need to set them to the end of input, i.e. the length of `data`.
    p_end = p_eof = lastindex(data)
    
    # We just use an empty dict here because we don't want our parser
    # to do anything just yet - only parse the input
    $(Automa.generate_exec_code(context, machine, actions))

    # We need to make sure that we reached the accept state, else the 
    # input did not parse correctly
    iszero(cs) || error("failed to parse on byte ", p)
    return nothing
end;

data = ">Feature V000017162_HA|swine|HA|H1N1|2020|V000017162|\n1\t1701\tgene\t\t\n\t\t\tgene\tHA\n1\t1701\tCDS\t\t\n\t\t\tproduct\themagglutinin\n\t\t\tprotein_id\tlcl|1samplep1\n\t\t\tfunction\treceptor binding and fusion protein\n\t\t\tgene\tHA\n1\t51\tsig_peptide\n52\t1032\tmat_peptide\n\t\t\tproduct HA1\n1033\t1698\tmat_peptide\n\t\t\tproduct HA2"

end # module
