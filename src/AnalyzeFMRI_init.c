#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void cluster_mass(void *, void *, void *, void *, void *, void *, void *);
extern void covariance_est(void *, void *, void *, void *, void *, void *);
extern void ica_fmri_JM(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void non_lin_gauss_smooth(void *, void *, void *, void *, void *, void *, void *);
extern void read_analyze_header_wrap_JM(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void read_nifti_header_wrap_JM(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void read_nifti_magic_wrap(void *, void *, void *);
extern void read2byte_v1_JM(void *, void *, void *, void *, void *, void *);
extern void read4byte_v1_JM(void *, void *, void *, void *, void *, void *);
extern void readchar_v1_JM(void *, void *, void *, void *, void *, void *);
extern void readdouble_v1_JM(void *, void *, void *, void *, void *, void *);
extern void readfloat_v1_JM(void *, void *, void *, void *, void *, void *);
extern void sim_grf(void *, void *, void *, void *, void *, void *, void *);
extern void spatial_mixture(void *, void *, void *, void *, void *, void *, void *, void *);
extern void swaptest_wrap_JM(void *, void *);
extern void temporal_non_lin_gauss_smooth(void *, void *, void *, void *, void *, void *, void *);
extern void write_analyze_header_wrap_JM(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void write_nifti_header_wrap_JM(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void write2byte_JM(void *, void *, void *);
extern void write2byteappend_JM(void *, void *, void *);
extern void write8bit_JM(void *, void *, void *);
extern void write8bitappend_JM(void *, void *, void *);
extern void writefloat_JM(void *, void *, void *);
extern void writefloatappend_JM(void *, void *, void *);

/* .Fortran calls */
extern void F77_NAME(gaussfilter1)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(gaussfilter2)(void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
    {"cluster_mass",                  (DL_FUNC) &cluster_mass,                   7},
    {"covariance_est",                (DL_FUNC) &covariance_est,                 6},
    {"ica_fmri_JM",                   (DL_FUNC) &ica_fmri_JM,                   16},
    {"non_lin_gauss_smooth",          (DL_FUNC) &non_lin_gauss_smooth,           7},
    {"read_analyze_header_wrap_JM",   (DL_FUNC) &read_analyze_header_wrap_JM,   45},
    {"read_nifti_header_wrap_JM",     (DL_FUNC) &read_nifti_header_wrap_JM,     46},
    {"read_nifti_magic_wrap",         (DL_FUNC) &read_nifti_magic_wrap,          3},
    {"read2byte_v1_JM",               (DL_FUNC) &read2byte_v1_JM,                6},
    {"read4byte_v1_JM",               (DL_FUNC) &read4byte_v1_JM,                6},
    {"readchar_v1_JM",                (DL_FUNC) &readchar_v1_JM,                 6},
    {"readdouble_v1_JM",              (DL_FUNC) &readdouble_v1_JM,               6},
    {"readfloat_v1_JM",               (DL_FUNC) &readfloat_v1_JM,                6},
    {"sim_grf",                       (DL_FUNC) &sim_grf,                        7},
    {"spatial_mixture",               (DL_FUNC) &spatial_mixture,                8},
    {"swaptest_wrap_JM",              (DL_FUNC) &swaptest_wrap_JM,               2},
    {"temporal_non_lin_gauss_smooth", (DL_FUNC) &temporal_non_lin_gauss_smooth,  7},
    {"write_analyze_header_wrap_JM",  (DL_FUNC) &write_analyze_header_wrap_JM,  44},
    {"write_nifti_header_wrap_JM",    (DL_FUNC) &write_nifti_header_wrap_JM,    44},
    {"write2byte_JM",                 (DL_FUNC) &write2byte_JM,                  3},
    {"write2byteappend_JM",           (DL_FUNC) &write2byteappend_JM,            3},
    {"write8bit_JM",                  (DL_FUNC) &write8bit_JM,                   3},
    {"write8bitappend_JM",            (DL_FUNC) &write8bitappend_JM,             3},
    {"writefloat_JM",                 (DL_FUNC) &writefloat_JM,                  3},
    {"writefloatappend_JM",           (DL_FUNC) &writefloatappend_JM,            3},
    {NULL, NULL, 0}
};

static const R_FortranMethodDef FortranEntries[] = {
    {"gaussfilter1", (DL_FUNC) &F77_NAME(gaussfilter1), 9},
    {"gaussfilter2", (DL_FUNC) &F77_NAME(gaussfilter2), 9},
    {NULL, NULL, 0}
};

void R_init_AnalyzeFMRI(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
