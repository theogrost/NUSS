// [[Rcpp::depends(BH)]]
// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/xpressive/xpressive.hpp>
using namespace Rcpp;
namespace xp = boost::xpressive;

//' Perform inverse regex search (C++)
//'
//' This function takes character vector \code{patterns} with regex patterns
//' (or fixed strings),
//' and searches for match in the \code{x} string
//'
//' @param patterns a character vector of regex or fixed patterns.
//' @param x a string to search for the match.
//' @param fixed a logical, indicating whether patterns are fixed strings.
//' @return Logical vector of length as \code{patterns} with true if pattern
//' was found.
//' @examples
//' igrepl(c("today","b.* fox", "jumps over", "vigorous"),
//' "The quick brown fox jumps over the lazy dog", FALSE)
//' igrepl(c("today","brown fox", "jumps over", "vigorous"),
//' "The quick brown fox jumps over the lazy dog", TRUE)
//' @export
// [[Rcpp::export]]
LogicalVector igrepl(CharacterVector patterns,
                     std::string x,
                     bool fixed=false) {
    LogicalVector result (patterns.length());
    int e = patterns.length();

    if(fixed) {
        for(int i = 0; i < e; i++) {
            if(x.find(patterns[i]) != std::string::npos) {
                result[i] = true;
            }
        }
    } else {
        std::vector<xp::sregex> regex_patterns(patterns.length());
        for(unsigned int i = 0; i<patterns.length(); ++i) {
            if(xp::regex_search(x, xp::sregex::compile(std::string(patterns[i])))) {
                result[i] = true;
            }
        }
    }

    return(result);
}

void internal_unigram_sequence_segmentation_search(bool omit_zero, std::string sequence, std::string left, CharacterVector un_restricted, NumericVector id_restricted, NumericVector po_restricted, CharacterVector& hs_result, List& un_result, List& id_result, List& po_result, CharacterVector un_current = CharacterVector::create(), NumericVector id_current = NumericVector::create(), NumericVector po_current = NumericVector::create()) {
    for(unsigned int i=0; i<un_restricted.length(); ++i) {
        if(boost::starts_with(left, std::string(un_restricted[i]))) {
            CharacterVector cu_un = un_current;
            NumericVector cu_id = id_current;
            NumericVector cu_po = po_current;
            cu_un.push_back(un_restricted[i]);
            if((po_restricted[i]!=0)|(!omit_zero)) {
                cu_id.push_back(id_restricted[i]);
                cu_po.push_back(po_restricted[i]);
            }
            std::string cu_sequence = left.substr(std::string(un_restricted[i]).length());
            if(cu_sequence=="") {
                hs_result.push_back(sequence);
                un_result.push_back(cu_un);
                id_result.push_back(cu_id);
                po_result.push_back(cu_po);
            } else {
                internal_unigram_sequence_segmentation_search(omit_zero, sequence, cu_sequence, un_restricted, id_restricted, po_restricted, hs_result, un_result, id_result,  po_result, cu_un, cu_id, cu_po);
            }
        }
    }
}

// [[Rcpp::export]]
List internal_unigram_sequence_segmentation(std::vector<std::string> sequences, CharacterVector to_search, CharacterVector to_replace, NumericVector ids, NumericVector points, bool omit_zero) {

    CharacterVector hs_result = CharacterVector::create();
    List un_result = List::create();
    List id_result = List::create();
    List po_result = List::create();

    for(unsigned int i = 0; i < sequences.size(); ++i) {
        std::string sequence = sequences[i];
        std::string no_sequence;
        no_sequence = sequence;

        LogicalVector sub = igrepl(to_search, sequence, true);

        CharacterVector un_restricted = to_replace[sub];
        NumericVector id_restricted = ids[sub];
        NumericVector po_restricted = points[sub];


        internal_unigram_sequence_segmentation_search(omit_zero, sequence, no_sequence, un_restricted, id_restricted, po_restricted, hs_result, un_result, id_result, po_result);

    }

    List result = List::create(Named("text")=hs_result, Named("segmented")=un_result, Named("ids")=id_result, Named("points")=po_result);
    return result;
}
