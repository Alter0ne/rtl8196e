// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// ulaalgo.h
//

#ifndef ULAALGO_H_2E403D182E83FB596AFB800E68B255A1
#define ULAALGO_H_2E403D182E83FB596AFB800E68B255A1

#include "umatrix.h"
#include "simd.h"

namespace ustl {

template <size_t NX, size_t NY, typename T>
void load_identity (matrix<NX,NY,T>& m)
{
    fill_n (m.begin(), NX * NY, 0);
    for (typename matrix<NX,NY,T>::iterator i = m.begin(); i < m.end(); i += NX + 1)
	*i = 1;
}

template <size_t NX, size_t NY, typename T>
matrix<NY,NY,T> operator* (const matrix<NX,NY,T>& m1, const matrix<NY,NX,T>& m2)
{
    matrix<NY,NY,T> mr;
    for (uoff_t ry = 0; ry < NY; ++ ry) {
	for (uoff_t rx = 0; rx < NY; ++ rx) {
	    T dpv (0);
	    for (uoff_t x = 0; x < NX; ++ x)
		dpv += m1[ry][x] * m2[x][rx];
	    mr[ry][rx] = dpv;
	}
    }
    return (mr);
}

template <size_t NX, size_t NY, typename T>
tuple<NX,T> operator* (const tuple<NY,T>& t, const matrix<NX,NY,T>& m)
{
    tuple<NX,T> tr;
    for (uoff_t x = 0; x < NX; ++ x) {
	T dpv (0);
	for (uoff_t y = 0; y < NY; ++ y)
	    dpv += t[y] * m[y][x];
	tr[x] = dpv;
    }
    return (tr);
}

template <size_t N, typename T>
void transpose (matrix<N,N,T>& m)
{
    for (uoff_t x = 0; x < N; ++ x)
	for (uoff_t y = x; y < N; ++ y)
	    swap (m[x][y], m[y][x]);
}

#if WANT_UNROLLED_COPY

#if CPU_HAS_SSE

template <>
inline void load_identity (matrix<4,4,float>& m)
{
    static const float one (1.0);
    asm volatile (
	"movups %1, (%0)		\n\t"	// 1 0 0 0
	"movaps %1, %%xmm1		\n\t"	// 1 0 0 0
	"shufps $0xB1,%%xmm1,%%xmm1	\n\t"	// 0 1 0 0
	"movups %%xmm1, 16(%0)		\n\t"	// 0 1 0 0
	"shufps $0x4F,%1,%%xmm1		\n\t"	// 0 0 1 0
	"shufps $0x1B,%1,%1		\n\t"	// 0 0 0 1
	"movups %%xmm1, 32(%0)		\n\t"	// 0 0 1 0
	"movups %1, 48(%0)"			// 0 0 0 1
	:
	: "r"(m.begin()), "x"(one)
	: "xmm0", "xmm1", "memory"
    );
}

inline void _sse_load_matrix (const float* m)
{
    asm volatile (
	"movups (%0), %%xmm4		\n\t" // xmm4 = m[1 2 3 4]
	"movups 16(%0), %%xmm5		\n\t" // xmm5 = m[1 2 3 4]
	"movups 32(%0), %%xmm6		\n\t" // xmm6 = m[1 2 3 4]
	"movups 48(%0), %%xmm7		\n\t" // xmm7 = m[1 2 3 4]
	: : "r"(m) : "xmm4", "xmm5", "xmm6", "xmm7"
    );
}

inline void _sse_transform_to_vector (float* result)
{
    asm volatile (
	"movaps %%xmm0, %%xmm1		\n\t" // xmm1 = t[0 1 2 3]
	"movaps %%xmm0, %%xmm2		\n\t" // xmm1 = t[0 1 2 3]
	"movaps %%xmm0, %%xmm3		\n\t" // xmm1 = t[0 1 2 3]
	"shufps $0x00, %%xmm0, %%xmm0	\n\t" // xmm0 = t[0 0 0 0]
	"shufps $0x66, %%xmm1, %%xmm1	\n\t" // xmm1 = t[1 1 1 1]
	"shufps $0xAA, %%xmm2, %%xmm2	\n\t" // xmm2 = t[2 2 2 2]
	"shufps $0xFF, %%xmm3, %%xmm3	\n\t" // xmm3 = t[3 3 3 3]
	"mulps  %%xmm4, %%xmm0		\n\t" // xmm0 = t[0 0 0 0] * m[0 1 2 3]
	"mulps  %%xmm5, %%xmm1		\n\t" // xmm1 = t[1 1 1 1] * m[0 1 2 3]
	"addps  %%xmm1, %%xmm0		\n\t" // xmm0 = xmm0 + xmm1
	"mulps  %%xmm6, %%xmm2		\n\t" // xmm2 = t[2 2 2 2] * m[0 1 2 3]
	"mulps  %%xmm7, %%xmm3		\n\t" // xmm3 = t[3 3 3 3] * m[0 1 2 3]
	"addps  %%xmm3, %%xmm2		\n\t" // xmm2 = xmm2 + xmm3
	"addps  %%xmm2, %%xmm0		\n\t" // xmm0 = result
	"movups %%xmm0, (%0)"
	:
	: "r"(result)
	: "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "memory"
    );
}

template <>
inline tuple<4,float> operator* (const tuple<4,float>& t, const matrix<4,4,float>& m)
{
    tuple<4,float> result;
    _sse_load_matrix (m.begin());
    asm volatile ("movups %0, %%xmm0	\n\t" : : "m"(t[0]) : "xmm0");
    _sse_transform_to_vector (result.begin());
    return (result);
}

template <>
inline matrix<4,4,float> operator* (const matrix<4,4,float>& m1, const matrix<4,4,float>& m2)
{
    matrix<4,4,float> result;
    _sse_load_matrix (m2.begin());
    asm volatile ("movups %0, %%xmm0\n\t" : : "m"(m1[0][0]) : "xmm0");
    _sse_transform_to_vector (result[0]);
    asm volatile ("movups %0, %%xmm0\n\t" : : "m"(m1[1][0]) : "xmm0");
    _sse_transform_to_vector (result[1]);
    asm volatile ("movups %0, %%xmm0\n\t" : : "m"(m1[2][0]) : "xmm0");
    _sse_transform_to_vector (result[2]);
    asm volatile ("movups %0, %%xmm0\n\t" : : "m"(m1[3][0]) : "xmm0");
    _sse_transform_to_vector (result[3]);
    return (result);
}

#elif CPU_HAS_3DNOW

/// Specialization for 4-component vector transform, the slow part of 3D graphics.
template <>
inline tuple<4,float> operator* (const tuple<4,float>& t, const matrix<4,4,float>& m)
{
    tuple<4,float> result;
    // This is taken from "AMD Athlon Code Optimization Guide" from AMD. 18 cycles!
    // If you are writing a 3D engine, you may want to copy it instead of calling it
    // because of the femms instruction at the end, which takes 2 cycles.
    asm volatile (
	"movq	   (%0), %%mm0		\n\t"	//            y | x
	"movq	   8(%0), %%mm1		\n\t"	//            w | z
	"movq	   %%mm0, %%mm2		\n\t"	//            y | x
	"movq	   (%1), %%mm3		\n\t"	//      m[0][1] | m[0][0]
	"punpckldq  %%mm0, %%mm0	\n\t"	//            x | x
	"movq	   16(%1), %%mm4	\n\t"	//      m[1][1] | m[1][0]
	"pfmul	   %%mm0, %%mm3		\n\t"	//    x*m[0][1] | x*m[0][0]
	"punpckhdq  %%mm2, %%mm2	\n\t"	//            y | y
	"pfmul	   %%mm2, %%mm4		\n\t"	//    y*m[1][1] | y*m[1][0]
	"movq	   8(%1), %%mm5		\n\t"	//      m[0][3] | m[0][2]
	"movq	   24(%1), %%mm7	\n\t"	//      m[1][3] | m[1][2]
	"movq	   %%mm1, %%mm6		\n\t"	//            w | z
	"pfmul	   %%mm0, %%mm5		\n\t"	//    x*m[0][3] | v0>x*m[0][2]
	"movq	   32(%1), %%mm0	\n\t"	//      m[2][1] | m[2][0]
	"punpckldq  %%mm1, %%mm1	\n\t"	//            z | z
	"pfmul	   %%mm2, %%mm7		\n\t"	//    y*m[1][3] | y*m[1][2]
	"movq	   40(%1), %%mm2	\n\t"	//      m[2][3] | m[2][2]
	"pfmul	   %%mm1, %%mm0		\n\t"	//    z*m[2][1] | z*m[2][0]
	"pfadd	   %%mm4, %%mm3		\n\t"	// x*m[0][1]+y*m[1][1] | x*m[0][0]+y*m[1][0]
	"movq	   48(%1), %%mm4	\n\t"	//      m[3][1] | m[3][0]
	"pfmul	   %%mm1, %%mm2		\n\t"	//    z*m[2][3] | z*m[2][2]
	"pfadd	   %%mm7, %%mm5		\n\t"	// x*m[0][3]+y*m[1][3] | x*m[0][2]+y*m[1][2]
	"movq	   56(%1), %%mm1	\n\t"	//      m[3][3] | m[3][2]
	"punpckhdq  %%mm6, %%mm6	\n\t"	//            w | w
	"pfadd	   %%mm0, %%mm3		\n\t"	// x*m[0][1]+y*m[1][1]+z*m[2][1] | x*m[0][0]+y*m[1][0]+z*m[2][0]
	"pfmul	   %%mm6, %%mm4		\n\t"	//    w*m[3][1] | w*m[3][0]
	"pfmul	   %%mm6, %%mm1		\n\t"	//    w*m[3][3] | w*m[3][2]
	"pfadd	   %%mm2, %%mm5		\n\t"	// x*m[0][3]+y*m[1][3]+z*m[2][3] | x*m[0][2]+y*m[1][2]+z*m[2][2]
	"pfadd	   %%mm4, %%mm3		\n\t"	// x*m[0][1]+y*m[1][1]+z*m[2][1]+w*m[3][1] | x*m[0][0]+y*m[1][0]+z*m[2][0]+w*m[3][0]
	"movq	   %%mm3, (%2)		\n\t"	// store result->y | result->x
	"pfadd	   %%mm1, %%mm5		\n\t"	// x*m[0][3]+y*m[1][3]+z*m[2][3]+w*m[3][3] | x*m[0][2]+y*m[1][2]+z*m[2][2]+w*m[3][2]
	"movq	   %%mm5, 8(%2)		\n\t"	// store result->w | result->z
	"femms				    "	// clear mmx state
	:
	: "r"(t.begin()), "r"(m.begin()), "r"(result.begin())
	: "memory","mm0","mm1","mm2","mm3","mm4","mm5","mm6","mm7"
    );
    return (result);
}

#else	// If no processor extensions, just unroll the multiplication

/// Specialization for 4-component vector transform, the slow part of 3D graphics.
template <>
inline tuple<4,float> operator* (const tuple<4,float>& t, const matrix<4,4,float>& m)
{
    tuple<4,float> tr;
    tr[0] = t[0] * m[0][0] + t[1] * m[1][0] + t[2] * m[2][0] + t[3] * m[3][0];
    tr[1] = t[0] * m[0][1] + t[1] * m[1][1] + t[2] * m[2][1] + t[3] * m[3][1];
    tr[2] = t[0] * m[0][2] + t[1] * m[1][2] + t[2] * m[2][2] + t[3] * m[3][2];
    tr[3] = t[0] * m[0][3] + t[1] * m[1][3] + t[2] * m[2][3] + t[3] * m[3][3];
    return (tr);
}

#endif	// CPU_HAS_3DNOW
#endif	// WANT_UNROLLED_COPY

} // namespace ustl

#endif

