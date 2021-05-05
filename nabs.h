/*
	nabs.h
	Copyright 2021, zhiayang

	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License.
*/

/*
	Version 0.1.0
	=============


	Documentation
	=============

	nabs is a single-header library for writing build recipes in C++. It is directly inspired by
	'nobuild' (https://github.com/tsoding/nobuild), but with more feature (bloat) and built-in support
	for makefile-like dependency resolution.

	The primary objective of this (not a) build system is to allow writing build recipes/scripts that:
	(a) are written in one language (the best one) -- C++
	(b) work for all major platforms -- Linux, macOS, BSDs, Windows
	(c) easily support, with very little boilerplate, standard makefile project patterns
	(d) provide platform- and compiler-independent abstractions for common flags/options

	More documentation will be coming soon.


	Version history is available at the bottom of this file.
*/

#pragma once

// this needs to appear above any C++ STL header.
#if defined(_WIN32)

#if defined(_HAS_EXCEPTIONS)
#undef _HAS_EXCEPTIONS
#endif

#define _HAS_EXCEPTIONS 0

#endif

/*
	zpr is included here to maintain a single-header strategy.
	it is available from https://github.com/zhiayang/ztl

	the code is included with the following modifications:
	1. documentation has been removed
	2. zpr::tt reimplementation of type_traits has been removed
	3. defines have been removed and set to their defaults.

	zpr.h
	Copyright 2020 - 2021, zhiayang

	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License.


	detail::print_floating and detail::print_exponent are adapted from _ftoa and _etoa
	from https://github.com/mpaland/printf, which is licensed under the MIT license,
	reproduced below:

	Copyright Marco Paland (info@paland.com), 2014-2019, PALANDesign Hannover, Germany

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in
	all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
	THE SOFTWARE.

	Version 2.3.1
	=============
*/

#include <cfloat>
#include <cstdio>
#include <cstring>
#include <cstddef>
#include <cstdint>

#include <string>
#include <string_view>
#include <type_traits>

#define ZPR_HEX_0X_RESPECTS_UPPERCASE   0
#define ZPR_FREESTANDING                0
#define ZPR_DECIMAL_LOOKUP_TABLE        1
#define ZPR_HEXADECIMAL_LOOKUP_TABLE    1
#define ZPR_USE_STD                     1

namespace zpr::tt
{
	using namespace std;

	template <typename A>
	A min(const A& a, const A& b)
	{
		return a < b ? a : b;
	}

	/* from here on, all the code should be exactly the same as the released version of zpr.h */
	/* -------------------------------------------------------------------------------------- */


	// is_any<X, A, B, ... Z> -> is_same<X, A> || is_same<X, B> || ...
	template <typename T, typename... Ts>
	struct is_any : disjunction<is_same<T, Ts>...> { };

	struct str_view
	{
		str_view() : ptr(nullptr), len(0) { }
		str_view(const char* p, size_t l) : ptr(p), len(l) { }

		template <size_t N>
		str_view(const char (&s)[N]) : ptr(s), len(N - 1) { }

		template <typename T, typename = tt::enable_if_t<tt::is_same_v<const char*, T>>>
		str_view(T s) : ptr(s), len(strlen(s)) { }

	#if ZPR_USE_STD

		str_view(const std::string& str) : ptr(str.data()), len(str.size()) { }
		str_view(const std::string_view& sv) : ptr(sv.data()), len(sv.size()) { }

	#endif

		str_view(str_view&&) = default;
		str_view(const str_view&) = default;
		str_view& operator= (str_view&&) = default;
		str_view& operator= (const str_view&) = default;

		inline bool operator== (const str_view& other) const
		{
			return (this->ptr == other.ptr && this->len == other.len)
				|| (strncmp(this->ptr, other.ptr, tt::min(this->len, other.len)) == 0);
		}

		inline bool operator!= (const str_view& other) const
		{
			return !(*this == other);
		}

		inline const char* begin() const { return this->ptr; }
		inline const char* end() const { return this->ptr + len; }

		inline size_t size() const { return this->len; }
		inline bool empty() const { return this->len == 0; }
		inline const char* data() const { return this->ptr; }

		inline char operator[] (size_t n) { return this->ptr[n]; }

		inline str_view drop(size_t n) const { return (this->size() >= n ? this->substr(n, this->size() - n) : ""); }
		inline str_view take(size_t n) const { return (this->size() >= n ? this->substr(0, n) : *this); }
		inline str_view take_last(size_t n) const { return (this->size() >= n ? this->substr(this->size() - n, n) : *this); }
		inline str_view drop_last(size_t n) const { return (this->size() >= n ? this->substr(0, this->size() - n) : *this); }

		inline str_view& remove_prefix(size_t n) { return (*this = this->drop(n)); }
		inline str_view& remove_suffix(size_t n) { return (*this = this->drop_last(n)); }

	private:
		inline str_view substr(size_t pos, size_t cnt) const { return str_view(this->ptr + pos, cnt); }

		const char* ptr;
		size_t len;
	};
}

namespace zpr
{
	constexpr uint8_t FMT_FLAG_ZERO_PAD         = 0x1;
	constexpr uint8_t FMT_FLAG_ALTERNATE        = 0x2;
	constexpr uint8_t FMT_FLAG_PREPEND_PLUS     = 0x4;
	constexpr uint8_t FMT_FLAG_PREPEND_SPACE    = 0x8;
	constexpr uint8_t FMT_FLAG_HAVE_WIDTH       = 0x10;
	constexpr uint8_t FMT_FLAG_HAVE_PRECISION   = 0x20;
	constexpr uint8_t FMT_FLAG_WIDTH_NEGATIVE   = 0x40;

	struct format_args
	{
		char specifier      = -1;
		uint8_t flags       = 0;

		int64_t width       = -1;
		int64_t length      = -1;
		int64_t precision   = -1;

		bool zero_pad() const       { return this->flags & FMT_FLAG_ZERO_PAD; }
		bool alternate() const      { return this->flags & FMT_FLAG_ALTERNATE; }
		bool have_width() const     { return this->flags & FMT_FLAG_HAVE_WIDTH; }
		bool have_precision() const { return this->flags & FMT_FLAG_HAVE_PRECISION; }
		bool prepend_plus() const   { return this->flags & FMT_FLAG_PREPEND_PLUS; }
		bool prepend_space() const  { return this->flags & FMT_FLAG_PREPEND_SPACE; }

		bool negative_width() const { return have_width() && (this->flags & FMT_FLAG_WIDTH_NEGATIVE); }
		bool positive_width() const { return have_width() && !negative_width(); }

		void set_precision(int64_t p)
		{
			this->precision = p;
			this->flags |= FMT_FLAG_HAVE_PRECISION;
		}

		void set_width(int64_t w)
		{
			this->width = w;
			this->flags |= FMT_FLAG_HAVE_WIDTH;

			if(w < 0)
				this->flags |= FMT_FLAG_WIDTH_NEGATIVE;
		}
	};

	template <typename T, typename = void>
	struct print_formatter { };


	namespace detail
	{
		template <typename T>
		struct __fmtarg_w
		{
			__fmtarg_w(__fmtarg_w&&) = delete;
			__fmtarg_w(const __fmtarg_w&) = delete;
			__fmtarg_w& operator= (__fmtarg_w&&) = delete;
			__fmtarg_w& operator= (const __fmtarg_w&) = delete;

			__fmtarg_w(T&& x, int width) : arg(static_cast<T&&>(x)), width(width) { }

			T arg;
			int width;
		};

		template <typename T>
		struct __fmtarg_p
		{
			__fmtarg_p(__fmtarg_p&&) = delete;
			__fmtarg_p(const __fmtarg_p&) = delete;
			__fmtarg_p& operator= (__fmtarg_p&&) = delete;
			__fmtarg_p& operator= (const __fmtarg_p&) = delete;

			__fmtarg_p(T&& x, int prec) : arg(static_cast<T&&>(x)), prec(prec) { }

			T arg;
			int prec;
		};

		template <typename T>
		struct __fmtarg_wp
		{
			__fmtarg_wp(__fmtarg_wp&&) = delete;
			__fmtarg_wp(const __fmtarg_wp&) = delete;
			__fmtarg_wp& operator= (__fmtarg_wp&&) = delete;
			__fmtarg_wp& operator= (const __fmtarg_wp&) = delete;

			__fmtarg_wp(T&& x, int width, int prec) : arg(static_cast<T&&>(x)), prec(prec), width(width) { }

			T arg;
			int prec;
			int width;
		};

		struct __fmtarg_w_helper
		{
			__fmtarg_w_helper(int w) : width(w) { }

			template <typename T> inline __fmtarg_w<T&&> operator() (T&& val)
			{
				return __fmtarg_w<T&&>(static_cast<T&&>(val), this->width);
			}

			int width;
		};

		struct __fmtarg_p_helper
		{
			__fmtarg_p_helper(int p) : prec(p) { }

			template <typename T> inline __fmtarg_p<T&&> operator() (T&& val)
			{
				return __fmtarg_p<T&&>(static_cast<T&&>(val), this->prec);
			}

			int prec;
		};

		struct __fmtarg_wp_helper
		{
			__fmtarg_wp_helper(int w, int p) : width(w), prec(p) { }

			template <typename T> inline __fmtarg_wp<T&&> operator() (T&& val)
			{
				return __fmtarg_wp<T&&>(static_cast<T&&>(val), this->width, this->prec);
			}

			int width;
			int prec;
		};

		struct dummy_appender
		{
			void operator() (char c);
			void operator() (tt::str_view sv);
			void operator() (char c, size_t n);
			void operator() (const char* begin, const char* end);
			void operator() (const char* begin, size_t len);
		};

		template <typename T, typename = void>
		struct has_formatter : tt::false_type { };

		template <typename T>
		struct has_formatter<T, tt::void_t<decltype(tt::declval<print_formatter<T>>()
			.print(tt::declval<T>(), dummy_appender(), { }))>
		> : tt::true_type { };

		template <typename T>
		constexpr bool has_formatter_v = has_formatter<T>::value;


		template <typename T, typename = void>
		struct is_iterable : tt::false_type { };

		template <typename T>
		struct is_iterable<T, tt::void_t<
			decltype(begin(tt::declval<T&>())),
			decltype(end(tt::declval<T&>()))
		>> : tt::true_type { };

		static inline format_args parse_fmt_spec(tt::str_view sv)
		{
			// remove the first and last (they are { and })
			sv = sv.drop(1).drop_last(1);

			format_args fmt_args = { };
			{
				while(sv.size() > 0)
				{
					switch(sv[0])
					{
						case '0':   fmt_args.flags |= FMT_FLAG_ZERO_PAD; sv.remove_prefix(1); continue;
						case '#':   fmt_args.flags |= FMT_FLAG_ALTERNATE; sv.remove_prefix(1); continue;
						case '-':   fmt_args.flags |= FMT_FLAG_WIDTH_NEGATIVE; sv.remove_prefix(1); continue;
						case '+':   fmt_args.flags |= FMT_FLAG_PREPEND_PLUS; sv.remove_prefix(1); continue;
						case ' ':   fmt_args.flags |= FMT_FLAG_PREPEND_SPACE; sv.remove_prefix(1); continue;
						default:    break;
					}

					break;
				}

				if(sv.empty())
					goto done;

				if('0' <= sv[0] && sv[0] <= '9')
				{
					fmt_args.flags |= FMT_FLAG_HAVE_WIDTH;

					size_t k = 0;
					fmt_args.width = 0;

					while(sv.size() > k && ('0' <= sv[k] && sv[k] <= '9'))
						(fmt_args.width = 10 * fmt_args.width + (sv[k] - '0')), k++;

					sv.remove_prefix(k);
				}

				if(sv.empty())
					goto done;

				if(sv.size() >= 2 && sv[0] == '.')
				{
					sv.remove_prefix(1);

					if(sv[0] == '-')
					{
						// just ignore negative precision i guess.
						size_t k = 1;
						while(sv.size() > k && ('0' <= sv[k] && sv[k] <= '9'))
							k++;

						sv.remove_prefix(k);
					}
					else if('0' <= sv[0] && sv[0] <= '9')
					{
						fmt_args.flags |= FMT_FLAG_HAVE_PRECISION;
						fmt_args.precision = 0;

						size_t k = 0;
						while(sv.size() > k && ('0' <= sv[k] && sv[k] <= '9'))
							(fmt_args.precision = 10 * fmt_args.precision + (sv[k] - '0')), k++;

						sv.remove_prefix(k);
					}
				}

				if(!sv.empty())
					fmt_args.specifier = sv[0];
			}

		done:
			return fmt_args;
		}

		template <typename CallbackFn>
		size_t print_string(CallbackFn&& cb, const char* str, size_t len, format_args args)
		{
			int64_t string_length = 0;

			if(args.have_precision())   string_length = tt::min(args.precision, static_cast<int64_t>(len));
			else                        string_length = static_cast<int64_t>(len);

			size_t ret = string_length;
			auto padding_width = args.width - string_length;

			if(args.positive_width() && padding_width > 0)
				cb(args.zero_pad() ? '0' : ' ', padding_width), ret += padding_width;

			cb(str, string_length);

			if(args.negative_width() && padding_width > 0)
				cb(args.zero_pad() ? '0' : ' ', padding_width), ret += padding_width;

			return ret;
		}

		template <typename CallbackFn>
		size_t print_special_floating(CallbackFn&& cb, double value, format_args args)
		{
			// uwu. apparently, `inf` and `nan` are never truncated.
			args.set_precision(999);

			if(value != value)
				return print_string(cb, "nan", 3, static_cast<format_args&&>(args));

			if(value < -DBL_MAX)
				return print_string(cb, "-inf", 4, static_cast<format_args&&>(args));

			if(value > DBL_MAX)
			{
				return print_string(cb, args.prepend_plus()
					? "+inf" : args.prepend_space()
					? " inf" : "inf",
					args.prepend_space() || args.prepend_plus() ? 4 : 3,
					static_cast<format_args&&>(args)
				);
			}

			return 0;
		}




		// forward declare these
		template <typename CallbackFn>
		size_t print_floating(CallbackFn&& cb, double value, format_args args);

		template <typename T>
		char* print_decimal_integer(char* buf, size_t bufsz, T value);



		template <typename CallbackFn>
		size_t print_exponent(CallbackFn&& cb, double value, format_args args)
		{
			constexpr int DEFAULT_PRECISION = 6;

			// check for NaN and special values
			if((value != value) || (value > DBL_MAX) || (value < -DBL_MAX))
				return print_special_floating(cb, value, static_cast<format_args&&>(args));

			int prec = (args.have_precision() ? static_cast<int>(args.precision) : DEFAULT_PRECISION);

			bool use_precision  = args.have_precision();
			bool use_zero_pad   = args.zero_pad() && args.positive_width();
			bool use_right_pad  = !use_zero_pad && args.negative_width();
			// bool use_left_pad   = !use_zero_pad && args.positive_width();

			// determine the sign
			const bool negative = (value < 0);
			if(negative)
				value = -value;

			// determine the decimal exponent
			// based on the algorithm by David Gay (https://www.ampl.com/netlib/fp/dtoa.c)
			union {
				uint64_t U;
				double F;
			} conv;

			conv.F = value;
			auto exp2 = static_cast<int64_t>((conv.U >> 52U) & 0x07FFU) - 1023; // effectively log2
			conv.U = (conv.U & ((1ULL << 52U) - 1U)) | (1023ULL << 52U);        // drop the exponent so conv.F is now in [1,2)

			// now approximate log10 from the log2 integer part and an expansion of ln around 1.5
			auto expval = static_cast<int64_t>(0.1760912590558 + exp2 * 0.301029995663981 + (conv.F - 1.5) * 0.289529654602168);

			// now we want to compute 10^expval but we want to be sure it won't overflow
			exp2 = static_cast<int64_t>(expval * 3.321928094887362 + 0.5);

			const double z = expval * 2.302585092994046 - exp2 * 0.6931471805599453;
			const double z2 = z * z;

			conv.U = static_cast<uint64_t>(exp2 + 1023) << 52U;

			// compute exp(z) using continued fractions, see https://en.wikipedia.org/wiki/Exponential_function#Continued_fractions_for_ex
			conv.F *= 1 + 2 * z / (2 - z + (z2 / (6 + (z2 / (10 + z2 / 14)))));

			// correct for rounding errors
			if(value < conv.F)
			{
				expval--;
				conv.F /= 10;
			}

			// the exponent format is "%+02d" and largest value is "307", so set aside 4-5 characters (including the e+ part)
			int minwidth = (-100 < expval && expval < 100) ? 4U : 5U;

			// in "%g" mode, "prec" is the number of *significant figures* not decimals
			if(args.specifier == 'g' || args.specifier == 'G')
			{
				// do we want to fall-back to "%f" mode?
				if((value >= 1e-4) && (value < 1e6))
				{
					if(static_cast<int64_t>(prec) > expval)
						prec = static_cast<uint64_t>(static_cast<int64_t>(prec) - expval - 1);

					else
						prec = 0;

					args.precision = prec;

					// no characters in exponent
					minwidth = 0;
					expval = 0;
				}
				else
				{
					// we use one sigfig for the whole part
					if(prec > 0 && use_precision)
						prec -= 1;
				}
			}

			// will everything fit?
			uint64_t fwidth = args.width;
			if(args.width > minwidth)
			{
				// we didn't fall-back so subtract the characters required for the exponent
				fwidth -= minwidth;
			}
			else
			{
				// not enough characters, so go back to default sizing
				fwidth = 0;
			}

			if(use_right_pad && minwidth)
			{
				// if we're padding on the right, DON'T pad the floating part
				fwidth = 0;
			}

			// rescale the float value
			if(expval)
				value /= conv.F;

			// output the floating part

			auto args_copy = args;
			args_copy.width = fwidth;
			auto len = static_cast<int64_t>(print_floating(cb, negative ? -value : value, args_copy));

			// output the exponent part
			if(minwidth > 0)
			{
				len++;
				if(args.specifier & 0x20)   cb('e');
				else                        cb('E');

				// output the exponent value
				char digits_buf[8] = { };
				size_t digits_len = 0;

				auto buf = print_decimal_integer(digits_buf, 8, static_cast<int64_t>(tt::abs(expval)));
				digits_len = 8 - (buf - digits_buf);

				len += digits_len + 1;
				cb(expval < 0 ? '-' : '+');

				// zero-pad to minwidth - 2
				if(auto tmp = (minwidth - 2) - static_cast<int>(digits_len); tmp > 0)
					len += tmp, cb('0', tmp);

				cb(buf, digits_len);

				// might need to right-pad spaces
				if(use_right_pad && args.width > len)
					cb(' ', args.width - len), len = args.width;
			}

			return len;
		}


		template <typename CallbackFn>
		size_t print_floating(CallbackFn&& cb, double value, format_args args)
		{
			constexpr int DEFAULT_PRECISION = 6;
			constexpr size_t MAX_BUFFER_LEN = 128;
			constexpr long double EXPONENTIAL_CUTOFF = 1e15;

			char buf[MAX_BUFFER_LEN] = { 0 };

			size_t len = 0;

			int prec = (args.have_precision() ? static_cast<int>(args.precision) : DEFAULT_PRECISION);

			bool use_zero_pad   = args.zero_pad() && args.positive_width();
			bool use_left_pad   = !use_zero_pad && args.positive_width();
			bool use_right_pad  = !use_zero_pad && args.negative_width();

			// powers of 10
			constexpr double pow10[] = {
				1,
				10,
				100,
				1000,
				10000,
				100000,
				1000000,
				10000000,
				100000000,
				1000000000,
				10000000000,
				100000000000,
				1000000000000,
				10000000000000,
				100000000000000,
				1000000000000000,
				10000000000000000,
			};

			// test for special values
			if((value != value) || (value > DBL_MAX) || (value < -DBL_MAX))
				return print_special_floating(cb, value, static_cast<format_args&&>(args));

			// switch to exponential for large values.
			if((value > EXPONENTIAL_CUTOFF) || (value < -EXPONENTIAL_CUTOFF))
				return print_exponent(cb, value, static_cast<format_args&&>(args));

			// default to g.
			if(args.specifier == -1)
				args.specifier = 'g';

			// test for negative
			const bool negative = (value < 0);
			if(value < 0)
				value = -value;

			// limit precision to 16, cause a prec >= 17 can lead to overflow errors
			while((len < MAX_BUFFER_LEN) && (prec > 16))
			{
				buf[len++] = '0';
				prec--;
			}

			auto whole = static_cast<int64_t>(value);
			auto tmp = (value - whole) * pow10[prec];
			auto frac = static_cast<unsigned long>(tmp);

			double diff = tmp - frac;

			if(diff > 0.5)
			{
				frac += 1;

				// handle rollover, e.g. case 0.99 with prec 1 is 1.0
				if(frac >= pow10[prec])
				{
					frac = 0;
					whole += 1;
				}
			}
			else if(diff < 0.5)
			{
				// ?
			}
			else if((frac == 0U) || (frac & 1U))
			{
				// if halfway, round up if odd OR if last digit is 0
				frac += 1;
			}

			if(prec == 0U)
			{
				diff = value - static_cast<double>(whole);
				if((!(diff < 0.5) || (diff > 0.5)) && (whole & 1))
				{
					// exactly 0.5 and ODD, then round up
					// 1.5 -> 2, but 2.5 -> 2
					whole += 1;
				}
			}
			else
			{
				auto count = prec;

				bool flag = (args.specifier == 'g' || args.specifier == 'G');
				// now do fractional part, as an unsigned number
				while(len < MAX_BUFFER_LEN)
				{
					if(flag && (frac % 10) == 0)
						goto skip;

					flag = false;
					buf[len++] = static_cast<char>('0' + (frac % 10));

				skip:
					count -= 1;
					if(!(frac /= 10))
						break;
				}

				// add extra 0s
				while((len < MAX_BUFFER_LEN) && (count-- > 0))
					buf[len++] = '0';

				// add decimal
				if(len < MAX_BUFFER_LEN)
					buf[len++] = '.';
			}

			// do whole part, number is reversed
			while(len < MAX_BUFFER_LEN)
			{
				buf[len++] = static_cast<char>('0' + (whole % 10));
				if(!(whole /= 10))
					break;
			}

			// pad leading zeros
			if(use_zero_pad)
			{
				auto width = args.width;

				if(args.have_width() != 0 && (negative || args.prepend_plus() || args.prepend_space()))
					width--;

				while((len < static_cast<size_t>(width)) && (len < MAX_BUFFER_LEN))
					buf[len++] = '0';
			}

			if(len < MAX_BUFFER_LEN)
			{
				if(negative)
					buf[len++] = '-';

				else if(args.prepend_plus())
					buf[len++] = '+'; // ignore the space if the '+' exists

				else if(args.prepend_space())
					buf[len++] = ' ';
			}

			// reverse it.
			for(size_t i = 0; i < len / 2; i++)
				tt::swap(buf[i], buf[len - i - 1]);

			auto padding_width = tt::max(int64_t(0), args.width - static_cast<int64_t>(len));

			if(use_left_pad) cb(' ', padding_width);
			if(use_zero_pad) cb('0', padding_width);

			cb(buf, len);

			if(use_right_pad)
				cb(' ', padding_width);

			return len + ((use_left_pad || use_right_pad) ? padding_width : 0);
		}


		template <typename T>
		char* print_hex_integer(char* buf, size_t bufsz, T value)
		{
			static_assert(sizeof(T) <= 8);

		#if ZPR_HEXADECIMAL_LOOKUP_TABLE
			constexpr const char lookup_table[] =
				"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
				"202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f"
				"404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f"
				"606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f"
				"808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f"
				"a0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebf"
				"c0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedf"
				"e0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff";
		#endif

			constexpr auto hex_digit = [](int x) -> char {
				if(0 <= x && x <= 9)
					return '0' + x;

				return 'a' + x - 10;
			};

			char* ptr = buf + bufsz;

			// if we have the lookup table, do two digits at a time.
		#if ZPR_HEXADECIMAL_LOOKUP_TABLE

			constexpr auto copy = [](char* dst, const char* src) {
				memcpy(dst, src, 2);
			};

			while(value >= 0x100)
			{
				copy((ptr -= 2), &lookup_table[(value & 0xFF) * 2]);
				value /= 0x100;
			}

			if(value < 0x10)
				*(--ptr) = hex_digit(static_cast<int>(value));

			else
				copy((ptr -= 2), &lookup_table[value * 2]);

		#else

			do {
				*(--ptr) = hex_digit(value & 0xF);
				value /= 0x10;

			} while(value > 0);

		#endif

			return ptr;
		}

		template <typename T>
		char* print_binary_integer(char* buf, size_t bufsz, T value)
		{
			char* ptr = buf + bufsz;

			do {
				*(--ptr) = ('0' + (value & 1));
				value >>= 1;

			} while(value > 0);

			return ptr;
		}

		template <typename T>
		char* print_decimal_integer(char* buf, size_t bufsz, T value)
		{
			static_assert(sizeof(T) <= 8);

		#if ZPR_DECIMAL_LOOKUP_TABLE
			constexpr const char lookup_table[] =
				"000102030405060708091011121314151617181920212223242526272829"
				"303132333435363738394041424344454647484950515253545556575859"
				"606162636465666768697071727374757677787980818283848586878889"
				"90919293949596979899";
		#endif

			bool neg = false;
			if constexpr (tt::is_signed_v<T>)
			{
				neg = (value < 0);
				if(neg)
					value = -value;
			}

			char* ptr = buf + bufsz;

			// if we have the lookup table, do two digits at a time.
		#if ZPR_DECIMAL_LOOKUP_TABLE

			constexpr auto copy = [](char* dst, const char* src) {
				memcpy(dst, src, 2);
			};

			while(value >= 100)
			{
				copy((ptr -= 2), &lookup_table[(value % 100) * 2]);
				value /= 100;
			}

			if(value < 10)
				*(--ptr) = static_cast<char>(value + '0');

			else
				copy((ptr -= 2), &lookup_table[value * 2]);

		#else

			do {
				*(--ptr) = ('0' + (value % 10));
				value /= 10;

			} while(value > 0);

		#endif

			if(neg)
				*(--ptr) = '-';

			return ptr;
		}

		template <typename T>
		char* print_integer(char* buf, size_t bufsz, T value, int base)
		{
			if(base == 2)       return print_binary_integer(buf, bufsz, value);
			else if(base == 16) return print_hex_integer(buf, bufsz, value);
			else                return print_decimal_integer(buf, bufsz, value);
		}






		struct __print_state_t
		{
			size_t len;
			const char* fmt;
			const char* beg;
			const char* end;
		};

		template <typename CallbackFn, typename T>
		void print_one(CallbackFn&& cb, format_args fmt_args, T&& value)
		{
			using Decayed_T = tt::decay_t<T>;

			static_assert(has_formatter_v<T> || has_formatter_v<Decayed_T>,
				"no formatter for type");

			if constexpr (has_formatter<T>::value)
			{
				print_formatter<T>().print(static_cast<T&&>(value),
					static_cast<CallbackFn&&>(cb), static_cast<format_args&&>(fmt_args));
			}
			else
			{
				print_formatter<Decayed_T>().print(static_cast<T&&>(value),
					static_cast<CallbackFn&&>(cb), static_cast<format_args&&>(fmt_args));
			}
		}

		template <typename CallbackFn, typename T>
		void skip_fmts(__print_state_t* pst, CallbackFn&& cb, T&& value)
		{
			while((static_cast<size_t>(pst->end - pst->fmt) <= pst->len) && pst->end && *pst->end)
			{
				if(*pst->end == '{')
				{
					auto tmp = pst->end;

					// flush whatever we have first:
					cb(pst->beg, pst->end);
					if(pst->end[1] == '{')
					{
						cb('{');
						pst->end += 2;
						pst->beg = pst->end;
						continue;
					}

					while(pst->end[0] && pst->end[0] != '}')
						pst->end++;

					// owo
					if(!pst->end[0])
						return;

					pst->end++;

					auto fmt_spec = parse_fmt_spec(tt::str_view(tmp, pst->end - tmp));
					print_one(static_cast<CallbackFn&&>(cb), static_cast<format_args&&>(fmt_spec), static_cast<T&&>(value));

					pst->beg = pst->end;
					break;
				}
				else if(*pst->end == '}')
				{
					cb(pst->beg, pst->end + 1);

					// well... we don't need to escape }, but for consistency, we accept either } or }} to print one }.
					if(pst->end[1] == '}')
						pst->end++;

					pst->end++;
					pst->beg = pst->end;
				}
				else
				{
					pst->end++;
				}
			}
		}







		template <typename CallbackFn, typename... Args>
		void print(CallbackFn&& cb, tt::str_view sv, Args&&... args)
		{
			__print_state_t st;
			st.len = sv.size();
			st.fmt = sv.data();
			st.beg = sv.data();
			st.end = sv.data();

			(skip_fmts(&st, static_cast<CallbackFn&&>(cb), static_cast<Args&&>(args)), ...);

			// flush
			cb(st.beg, st.len - (st.beg - st.fmt));
		}

		template <size_t N, typename CallbackFn, typename... Args>
		void print(CallbackFn&& cb, const char (&fmt)[N], Args&&... args)
		{
			print(cb, tt::str_view(fmt, N - 1), static_cast<Args&&>(args)...);
		}



	#if ZPR_USE_STD
		struct string_appender
		{
			string_appender(std::string& buf) : buf(buf) { }

			inline void operator() (char c) { this->buf += c; }
			inline void operator() (tt::str_view sv) { this->buf += std::string_view(sv.data(), sv.size()); }
			inline void operator() (char c, size_t n) { this->buf.resize(this->buf.size() + n, c); }
			inline void operator() (const char* begin, const char* end) { this->buf.append(begin, end); }
			inline void operator() (const char* begin, size_t len) { this->buf.append(begin, begin + len); }

			string_appender(string_appender&&) = delete;
			string_appender(const string_appender&) = delete;
			string_appender& operator= (string_appender&&) = delete;
			string_appender& operator= (const string_appender&) = delete;

		private:
			std::string& buf;
		};
	#endif

	#if !ZPR_FREESTANDING
		template <size_t Limit, bool Newline>
		struct file_appender
		{
			file_appender(FILE* fd, size_t& written) : fd(fd), written(written) { }
			~file_appender() { flush(true); }

			file_appender(file_appender&&) = delete;
			file_appender(const file_appender&) = delete;
			file_appender& operator= (file_appender&&) = delete;
			file_appender& operator= (const file_appender&) = delete;

			inline void operator() (char c) { *ptr++ = c; flush(); }

			inline void operator() (tt::str_view sv) { (*this)(sv.data(), sv.size()); }
			inline void operator() (const char* begin, const char* end) { (*this)(begin, static_cast<size_t>(end - begin)); }

			inline void operator() (char c, size_t n)
			{
				while(n > 0)
				{
					auto x = tt::min(n, remaining());
					memset(ptr, c, x);
					ptr += x;
					n -= x;
					flush();
				}
			}

			inline void operator() (const char* begin, size_t len)
			{
				while(len > 0)
				{
					auto x = tt::min(len, remaining());
					memcpy(ptr, begin, x);
					ptr += x;
					begin += x;
					len -= x;

					flush();
				}
			}

		private:
			inline size_t remaining()
			{
				return Limit - (ptr - buf);
			}

			inline void flush(bool last = false)
			{
				if(!last && static_cast<size_t>(ptr - buf) < Limit)
				{
					if constexpr (Newline)
						this->buf[ptr - buf] = '\n';

					return;
				}

				if(!last || !Newline)
				{
					fwrite(buf, sizeof(char), ptr - buf, fd);
					written += ptr - buf;

					ptr = buf;
				}
				else if(last && Newline)
				{
					// here's a special trick -- write one extra, because we always ensure that
					// "one-past" the last character in our buffer is a newline.
					fwrite(buf, sizeof(char), ptr - buf + 1, fd);
					written += (ptr - buf) + 1;

					ptr = buf;
				}
			}

			FILE* fd = 0;

			char buf[Limit + 1];
			char* ptr = &buf[0];
			size_t& written;
		};
	#endif

		template <typename Fn>
		struct callback_appender
		{
			callback_appender(Fn* callback, bool newline) : len(0), newline(newline), callback(callback) { }
			~callback_appender() { if(newline) { (*callback)("\n", 1); } }

			inline void operator() (char c) { (*callback)(&c, 1); this->len += 1; }
			inline void operator() (tt::str_view sv) { (*callback)(sv.data(), sv.size()); this->len += sv.size(); }
			inline void operator() (const char* begin, const char* end) { (*callback)(begin, end - begin); this->len += (end - begin); }
			inline void operator() (const char* begin, size_t len) { (*callback)(begin, len); this->len += len; }
			inline void operator() (char c, size_t n)
			{
				for(size_t i = 0; i < n; i++)
					(*callback)(&c, 1);

				this->len += n;
			}

			callback_appender(callback_appender&&) = delete;
			callback_appender(const callback_appender&) = delete;
			callback_appender& operator= (callback_appender&&) = delete;
			callback_appender& operator= (const callback_appender&) = delete;

			size_t size() { return this->len; }

		private:
			size_t len;
			bool newline;
			Fn* callback;
		};



		struct buffer_appender
		{
			buffer_appender(char* buf, size_t cap) : buf(buf), cap(cap), len(0) { }

			inline void operator() (char c)
			{
				if(this->len < this->cap)
					this->buf[this->len++] = c;
			}

			inline void operator() (tt::str_view sv)
			{
				auto l = this->remaining(sv.size());
				memmove(&this->buf[this->len], sv.data(), l);
				this->len += l;
			}

			inline void operator() (char c, size_t n)
			{
				for(size_t i = 0; i < this->remaining(n); i++)
					this->buf[this->len++] = c;
			}

			inline void operator() (const char* begin, const char* end)
			{
				(*this)(tt::str_view(begin, end - begin));
			}

			inline void operator() (const char* begin, size_t len)
			{
				(*this)(tt::str_view(begin, len));
			}

			buffer_appender(buffer_appender&&) = delete;
			buffer_appender(const buffer_appender&) = delete;
			buffer_appender& operator= (buffer_appender&&) = delete;
			buffer_appender& operator= (const buffer_appender&) = delete;

			inline size_t size() { return this->len; }

		private:
			inline size_t remaining(size_t n) { return tt::min(this->cap - this->len, n); }

			char* buf = 0;
			size_t cap = 0;
			size_t len = 0;
		};

		constexpr size_t STDIO_BUFFER_SIZE = 4096;
	}




	template <typename CallbackFn, typename... Args>
	size_t cprint(CallbackFn&& callback, tt::str_view fmt, Args&&... args)
	{
		auto appender = detail::callback_appender(&callback, /* newline: */ false);
		detail::print(appender, fmt,
			static_cast<Args&&>(args)...);

		return appender.size();
	}

	template <typename CallbackFn, typename... Args>
	size_t cprintln(CallbackFn&& callback, tt::str_view fmt, Args&&... args)
	{
		auto appender = detail::callback_appender(&callback, /* newline: */ true);
		detail::print(appender, fmt,
			static_cast<Args&&>(args)...);

		return appender.size();
	}

	template <typename... Args>
	size_t sprint(size_t len, char* buf, tt::str_view fmt, Args&&... args)
	{
		auto appender = detail::buffer_appender(buf, len);
		detail::print(appender, fmt,
			static_cast<Args&&>(args)...);

		return appender.size();
	}



	// if we are freestanding, we don't have stdio, so we can't print to "stdout".
	#if !ZPR_FREESTANDING

		template <typename... Args>
		size_t print(tt::str_view fmt, Args&&... args)
		{
			size_t ret = 0;
			detail::print(detail::file_appender<detail::STDIO_BUFFER_SIZE, false>(stdout, ret), fmt,
				static_cast<Args&&>(args)...);

			return ret;
		}

		template <typename... Args>
		size_t println(tt::str_view fmt, Args&&... args)
		{
			size_t ret = 0;
			detail::print(detail::file_appender<detail::STDIO_BUFFER_SIZE, true>(stdout, ret), fmt,
				static_cast<Args&&>(args)...);

			return ret;
		}

		template <typename... Args>
		size_t fprint(FILE* file, tt::str_view fmt, Args&&... args)
		{
			size_t ret = 0;
			detail::print(detail::file_appender<detail::STDIO_BUFFER_SIZE, false>(file, ret), fmt,
				static_cast<Args&&>(args)...);

			return ret;
		}

		template <typename... Args>
		size_t fprintln(FILE* file, tt::str_view fmt, Args&&... args)
		{
			size_t ret = 0;
			detail::print(detail::file_appender<detail::STDIO_BUFFER_SIZE, true>(file, ret), fmt,
				static_cast<Args&&>(args)...);

			return ret;
		}
	#endif

	inline detail::__fmtarg_w_helper w(int width)
	{
		return detail::__fmtarg_w_helper(width);
	}

	inline detail::__fmtarg_p_helper p(int prec)
	{
		return detail::__fmtarg_p_helper(prec);
	}

	inline detail::__fmtarg_wp_helper wp(int width, int prec)
	{
		return detail::__fmtarg_wp_helper(width, prec);
	}


	// formatters lie here.
	template <typename T>
	struct print_formatter<detail::__fmtarg_w<T>>
	{
		template <typename Cb, typename F = detail::__fmtarg_w<T>>
		void print(F&& x, Cb&& cb, format_args args)
		{
			args.set_width(x.width);
			detail::print_one(static_cast<Cb&&>(cb), static_cast<format_args&&>(args), static_cast<T&&>(x.arg));
		}
	};

	template <typename T>
	struct print_formatter<detail::__fmtarg_p<T>>
	{
		template <typename Cb, typename F = detail::__fmtarg_p<T>>
		void print(F&& x, Cb&& cb, format_args args)
		{
			args.set_precision(x.prec);
			detail::print_one(static_cast<Cb&&>(cb), static_cast<format_args&&>(args), static_cast<T&&>(x.arg));
		}
	};

	template <typename T>
	struct print_formatter<detail::__fmtarg_wp<T>>
	{
		template <typename Cb, typename F = detail::__fmtarg_wp<T>>
		void print(F&& x, Cb&& cb, format_args args)
		{
			args.set_width(x.width);
			args.set_precision(x.prec);
			detail::print_one(static_cast<Cb&&>(cb), static_cast<format_args&&>(args), static_cast<T&&>(x.arg));
		}
	};

	namespace detail
	{
		template <typename T>
		struct __int_formatter
		{
			using Decayed_T = tt::decay_t<T>;

			template <typename Cb>
			void print(T x, Cb&& cb, format_args args)
			{
				int base = 10;
				if((args.specifier | 0x20) == 'x')  base = 16;
				else if(args.specifier == 'b')      base = 2;
				else if(args.specifier == 'p')
				{
					base = 16;
					args.specifier = 'x';
					args.flags |= FMT_FLAG_ALTERNATE;
				}

				// if we print base 2 we need 64 digits!
				constexpr size_t digits_buf_sz = 65;
				char digits_buf[digits_buf_sz] = { };

				char* digits = 0;
				size_t digits_len = 0;


				{
					if constexpr (tt::is_unsigned<Decayed_T>::value)
					{
						digits = detail::print_integer(digits_buf, digits_buf_sz, x, base);
						digits_len = digits_buf_sz - (digits - digits_buf);
					}
					else
					{
						if(base == 16)
						{
							digits = detail::print_integer(digits_buf, digits_buf_sz,
								static_cast<tt::make_unsigned_t<Decayed_T>>(x), base);

							digits_len = digits_buf_sz - (digits - digits_buf);
						}
						else
						{
							auto abs_val = tt::abs(x);
							digits = detail::print_integer(digits_buf, digits_buf_sz, abs_val, base);

							digits_len = digits_buf_sz - (digits - digits_buf);
						}
					}

					if('A' <= args.specifier && args.specifier <= 'Z')
						for(size_t i = 0; i < digits_len; i++)
							digits[i] = static_cast<char>(digits[i] - 0x20);
				}

				char prefix[4] = { 0 };
				int64_t prefix_len = 0;
				int64_t prefix_digits_length = 0;
				{
					char* pf = prefix;
					if(args.prepend_plus())
						prefix_len++, *pf++ = '+';

					else if(args.prepend_space())
						prefix_len++, *pf++ = ' ';

					else if(x < 0 && base == 10)
						prefix_len++, *pf++ = '-';

					if(base != 10 && args.alternate())
					{
						*pf++ = '0';
						*pf++ = (ZPR_HEX_0X_RESPECTS_UPPERCASE ? args.specifier : (args.specifier | 0x20));

						prefix_digits_length += 2;
						prefix_len += 2;
					}
				}

				int64_t output_length_with_precision = (args.have_precision()
					? tt::max(args.precision, static_cast<int64_t>(digits_len))
					: static_cast<int64_t>(digits_len)
				);

				int64_t total_digits_length = prefix_digits_length + static_cast<int64_t>(digits_len);
				int64_t normal_length = prefix_len + static_cast<int64_t>(digits_len);
				int64_t length_with_precision = prefix_len + output_length_with_precision;

				bool use_precision = args.have_precision();
				bool use_zero_pad  = args.zero_pad() && args.positive_width() && !use_precision;
				bool use_left_pad  = !use_zero_pad && args.positive_width();
				bool use_right_pad = !use_zero_pad && args.negative_width();

				int64_t padding_width = args.width - length_with_precision;
				int64_t zeropad_width = args.width - normal_length;
				int64_t precpad_width = args.precision - total_digits_length;

				if(padding_width <= 0) { use_left_pad = false; use_right_pad = false; }
				if(zeropad_width <= 0) { use_zero_pad = false; }
				if(precpad_width <= 0) { use_precision = false; }

				// pre-prefix
				if(use_left_pad) cb(' ', padding_width);

				cb(prefix, prefix_len);

				// post-prefix
				if(use_zero_pad) cb('0', zeropad_width);

				// prec-string
				if(use_precision) cb('0', precpad_width);

				cb(digits, digits_len);

				// postfix
				if(use_right_pad) cb(' ', padding_width);
			}
		};
	}

	template <> struct print_formatter<signed char> : detail::__int_formatter<signed char> { };
	template <> struct print_formatter<unsigned char> : detail::__int_formatter<unsigned char> { };
	template <> struct print_formatter<signed short> : detail::__int_formatter<signed short> { };
	template <> struct print_formatter<unsigned short> : detail::__int_formatter<unsigned short> { };
	template <> struct print_formatter<signed int> : detail::__int_formatter<signed int> { };
	template <> struct print_formatter<unsigned int> : detail::__int_formatter<unsigned int> { };
	template <> struct print_formatter<signed long> : detail::__int_formatter<signed long> { };
	template <> struct print_formatter<unsigned long> : detail::__int_formatter<unsigned long> { };
	template <> struct print_formatter<signed long long> : detail::__int_formatter<signed long long> { };
	template <> struct print_formatter<unsigned long long> : detail::__int_formatter<unsigned long long> { };

	template <typename T>
	struct print_formatter<T, typename tt::enable_if<(
		tt::is_enum_v<tt::decay_t<T>>
	)>::type>
	{
		template <typename Cb>
		void print(T x, Cb&& cb, format_args args)
		{
			using underlying = tt::underlying_type_t<tt::decay_t<T>>;
			detail::print_one(static_cast<Cb&&>(cb), static_cast<format_args&&>(args), static_cast<underlying>(x));
		}
	};

	template <>
	struct print_formatter<float>
	{
		template <typename Cb>
		void print(float x, Cb&& cb, format_args args)
		{
			if(args.specifier == 'e' || args.specifier == 'E')
				print_exponent(static_cast<Cb&&>(cb), x, static_cast<format_args&&>(args));

			else
				print_floating(static_cast<Cb&&>(cb), x, static_cast<format_args&&>(args));
		}

		template <typename Cb>
		void print(double x, Cb&& cb, format_args args)
		{
			if(args.specifier == 'e' || args.specifier == 'E')
				print_exponent(static_cast<Cb&&>(cb), x, static_cast<format_args&&>(args));

			else
				print_floating(static_cast<Cb&&>(cb), x, static_cast<format_args&&>(args));
		}
	};

	template <size_t N>
	struct print_formatter<const char (&)[N]>
	{
		template <typename Cb>
		void print(const char (&x)[N], Cb&& cb, format_args args)
		{
			detail::print_string(static_cast<Cb&&>(cb), x, N - 1, static_cast<format_args&&>(args));
		}
	};

	template <>
	struct print_formatter<const char*>
	{
		template <typename Cb>
		void print(const char* x, Cb&& cb, format_args args)
		{
			detail::print_string(static_cast<Cb&&>(cb), x, strlen(x), static_cast<format_args&&>(args));
		}
	};

	template <>
	struct print_formatter<char>
	{
		template <typename Cb>
		void print(char x, Cb&& cb, format_args args)
		{
			detail::print_string(static_cast<Cb&&>(cb), &x, 1, static_cast<format_args&&>(args));
		}
	};

	template <>
	struct print_formatter<bool>
	{
		template <typename Cb>
		void print(bool x, Cb&& cb, format_args args)
		{
			detail::print_string(static_cast<Cb&&>(cb),
				x ? "true" : "false",
				x ? 4      : 5,
				static_cast<format_args&&>(args)
			);
		}
	};

	template <>
	struct print_formatter<const void*>
	{
		template <typename Cb>
		void print(const void* x, Cb&& cb, format_args args)
		{
			args.specifier = 'p';
			print_one(static_cast<Cb&&>(cb), static_cast<format_args&&>(args), reinterpret_cast<uintptr_t>(x));
		}
	};

	template <typename T>
	struct print_formatter<T, typename tt::enable_if<(
		tt::conjunction<detail::is_iterable<T>, tt::is_same<tt::remove_cv_t<typename T::value_type>, char>>::value
	)>::type>
	{
		template <typename Cb>
		void print(const T& x, Cb&& cb, format_args args)
		{
			detail::print_string(static_cast<Cb&&>(cb), x.data(), x.size(), static_cast<format_args&&>(args));
		}
	};

	// exclude strings and string_views
	template <typename T>
	struct print_formatter<T, typename tt::enable_if<(
		tt::conjunction<detail::is_iterable<T>,
			tt::negation<tt::is_same<typename T::value_type, char>>
		>::value
	)>::type>
	{
		template <typename Cb>
		void print(const T& x, Cb&& cb, format_args args)
		{
			if(begin(x) == end(x))
			{
				if(!args.alternate())
					cb("[ ]");
				return;
			}

			if(!args.alternate())
				cb("[");

			for(auto it = begin(x);;)
			{
				detail::print_one(static_cast<Cb&&>(cb), args, *it);
				++it;

				if(it != end(x))
				{
					if(!args.alternate())
						cb(", ");
					else
						cb(" ");
				}
				else
				{
					break;
				}
			}

			if(!args.alternate())
				cb("]");
		}
	};



	template <>
	struct print_formatter<void*> : print_formatter<const void*> { };

	template <>
	struct print_formatter<char*> : print_formatter<const char*> { };

	template <>
	struct print_formatter<double> : print_formatter<float> { };

	template <size_t N>
	struct print_formatter<char (&)[N]> : print_formatter<const char (&)[N]> { };




	#if ZPR_USE_STD

		template <typename... Args>
		std::string sprint(tt::str_view fmt, Args&&... args)
		{
			std::string buf;
			detail::print(detail::string_appender(buf), fmt,
				static_cast<Args&&>(args)...);

			return buf;
		}

		template <typename A, typename B>
		struct print_formatter<std::pair<A, B>>
		{
			template <typename Cb>
			void print(const std::pair<A, B>& x, Cb&& cb, format_args args)
			{
				cb("{ ");
				detail::print_one(static_cast<Cb&&>(cb), args, x.first);
				cb(", ");
				detail::print_one(static_cast<Cb&&>(cb), args, x.second);
				cb(" }");
			}
		};
	#endif
}


/*
	the Result type is not (yet) available in standalone form, but it is
	included here because it seems like a nice, useful type that people
	would want to have. because CTAD (class template argument deduction)
	only works on class templates (and not template usings), we change
	the original namespace from `zst` to `nabs` so everyone has a good time.

	zst::Result (version 1.1)
	Copyright 2020 - 2021, zhiayang

	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License.


	Version 1.1 - 24/04/2021
	========================

	Updated from baseline with:
	1. expect()
*/
#include <cassert>
namespace nabs
{
	// we need a forward-declaration of error_and_exit for expect().
	template <typename... Args> [[noreturn]] void error_and_exit(std::string_view fmt, Args&&... args);

	template <typename T>
	struct Ok
	{
		Ok() = default;
		~Ok() = default;

		Ok(Ok&&) = delete;
		Ok(const Ok&) = delete;

		Ok(const T& value) : m_value(value) { }
		Ok(T&& value) : m_value(static_cast<T&&>(value)) { }

		template <typename... Args>
		Ok(Args&&... args) : m_value(static_cast<Args&&>(args)...) { }

		template <typename, typename> friend struct Result;

	private:
		T m_value;
	};

	template <>
	struct Ok<void>
	{
		Ok() = default;
		~Ok() = default;

		Ok(Ok&&) = delete;
		Ok(const Ok&) = delete;

		template <typename, typename> friend struct Result;
		template <typename, typename> friend struct zpr::print_formatter;
	};

	// deduction guide
	Ok() -> Ok<void>;


	template <typename E>
	struct Err
	{
		Err() = default;
		~Err() = default;

		Err(Err&&) = delete;
		Err(const Err&) = delete;

		Err(const E& error) : m_error(error) { }
		Err(E&& error) : m_error(static_cast<E&&>(error)) { }

		template <typename... Args>
		Err(Args&&... args) : m_error(static_cast<Args&&>(args)...) { }

		template <typename, typename> friend struct Result;
		template <typename, typename> friend struct zpr::print_formatter;

	private:
		E m_error;
	};

	template <typename T, typename E>
	struct Result
	{
	private:
		static constexpr int STATE_NONE = 0;
		static constexpr int STATE_VAL  = 1;
		static constexpr int STATE_ERR  = 2;

		struct tag_ok { };
		struct tag_err { };

		Result(tag_ok _, const T& x)  : state(STATE_VAL), val(x) { (void) _; }
		Result(tag_ok _, T&& x)       : state(STATE_VAL), val(static_cast<T&&>(x)) { (void) _; }

		Result(tag_err _, const E& e) : state(STATE_ERR), err(e) { (void) _; }
		Result(tag_err _, E&& e)      : state(STATE_ERR), err(static_cast<E&&>(e)) { (void) _; }

	public:
		using value_type = T;
		using error_type = E;

		~Result()
		{
			if(state == STATE_VAL) this->val.~T();
			if(state == STATE_ERR) this->err.~E();
		}

		template <typename T1 = T>
		Result(Ok<T1>&& ok) : Result(tag_ok(), static_cast<T1&&>(ok.m_value)) { }

		template <typename E1 = E>
		Result(Err<E1>&& err) : Result(tag_err(), static_cast<E1&&>(err.m_error)) { }

		Result(const Result& other)
		{
			this->state = other.state;
			if(this->state == STATE_VAL) new(&this->val) T(other.val);
			if(this->state == STATE_ERR) new(&this->err) E(other.err);
		}

		Result(Result&& other)
		{
			this->state = other.state;
			other.state = STATE_NONE;

			if(this->state == STATE_VAL) new(&this->val) T(static_cast<T&&>(other.val));
			if(this->state == STATE_ERR) new(&this->err) E(static_cast<E&&>(other.err));
		}

		Result& operator=(const Result& other)
		{
			if(this != &other)
			{
				if(this->state == STATE_VAL) this->val.~T();
				if(this->state == STATE_ERR) this->err.~E();

				this->state = other.state;
				if(this->state == STATE_VAL) new(&this->val) T(other.val);
				if(this->state == STATE_ERR) new(&this->err) E(other.err);
			}
			return *this;
		}

		Result& operator=(Result&& other)
		{
			if(this != &other)
			{
				if(this->state == STATE_VAL) this->val.~T();
				if(this->state == STATE_ERR) this->err.~E();

				this->state = other.state;
				other.state = STATE_NONE;

				if(this->state == STATE_VAL) new(&this->val) T(static_cast<T&&>(other.val));
				if(this->state == STATE_ERR) new(&this->err) E(static_cast<E&&>(other.err));
			}
			return *this;
		}

		T* operator -> () { this->assert_has_value(); return &this->val; }
		const T* operator -> () const { this->assert_has_value(); return &this->val; }

		T& operator* () { this->assert_has_value(); return this->val; }
		const T& operator* () const  { this->assert_has_value(); return this->val; }

		operator bool() const { return this->state == STATE_VAL; }
		bool ok() const { return this->state == STATE_VAL; }

		const T& unwrap() const { this->assert_has_value(); return this->val; }
		const E& error() const { this->assert_is_error(); return this->err; }

		T& unwrap() { this->assert_has_value(); return this->val; }
		E& error() { this->assert_is_error(); return this->err; }

		// enable implicit upcast to a base type
		template <typename U, typename = std::enable_if_t<
			std::is_pointer_v<T> && std::is_pointer_v<U>
			&& std::is_base_of_v<std::remove_pointer_t<U>, std::remove_pointer_t<T>>
		>>
		operator Result<U, E> () const
		{
			if(state == STATE_VAL)  return Result<U, E>(this->val);
			if(state == STATE_ERR)  return Result<U, E>(this->err);

			assert(false);
		}

		const T& expect(std::string_view msg) const
		{
			if(this->ok())  return this->unwrap();
			else            nabs::error_and_exit("{}: {}", msg, this->error());
		}

		const T& or_else(const T& default_value) const
		{
			if(this->ok())  return this->unwrap();
			else            return default_value;
		}

	private:
		inline void assert_has_value() const
		{
			if(this->state != STATE_VAL)
				error_and_exit("unwrapping result of Err: {}", this->error());
		}

		inline void assert_is_error() const
		{
			if(this->state != STATE_ERR)
				error_and_exit("result is not an Err");
		}


		// 0 = schrodinger -- no error, no value.
		// 1 = valid
		// 2 = error
		int state = 0;
		union {
			T val;
			E err;
		};
	};


	template <typename E>
	struct Result<void, E>
	{
	private:
		static constexpr int STATE_NONE = 0;
		static constexpr int STATE_VAL  = 1;
		static constexpr int STATE_ERR  = 2;

	public:
		using value_type = void;
		using error_type = E;

		Result() : state(STATE_VAL) { }

		Result(Ok<void>&& ok) : Result() { (void) ok; }

		template <typename E1 = E>
		Result(Err<E1>&& err) : state(STATE_ERR), err(static_cast<E1&&>(err.m_error)) { }


		Result(const Result& other)
		{
			this->state = other.state;
			if(this->state == STATE_ERR)
				this->err = other.err;
		}

		Result(Result&& other)
		{
			this->state = other.state;
			other.state = STATE_NONE;

			if(this->state == STATE_ERR)
				this->err = static_cast<E&&>(other.err);
		}

		Result& operator=(const Result& other)
		{
			if(this != &other)
			{
				this->state = other.state;
				if(this->state == STATE_ERR)
					this->err = other.err;
			}
			return *this;
		}

		Result& operator=(Result&& other)
		{
			if(this != &other)
			{
				this->state = other.state;
				other.state = STATE_NONE;

				if(this->state == STATE_ERR)
					this->err = static_cast<E&&>(other.err);
			}
			return *this;
		}

		operator bool() const { return this->state == STATE_VAL; }
		bool ok() const { return this->state == STATE_VAL; }

		const E& error() const { this->assert_is_error(); return this->err; }
		E& error() { this->assert_is_error(); return this->err; }

		void expect(std::string_view msg) const
		{
			if(!this->ok())
				nabs::error_and_exit("{}: {}", msg, this->error());
		}

		static Result of_value()
		{
			return Result<void, E>();
		}

		template <typename E1 = E>
		static Result of_error(E1&& err)
		{
			return Result<void, E>(E(static_cast<E1&&>(err)));
		}

		template <typename... Args>
		static Result of_error(Args&&... xs)
		{
			return Result<void, E>(E(static_cast<Args&&>(xs)...));
		}

	private:
		inline void assert_is_error() const
		{
			if(this->state != STATE_ERR)
				error_and_exit("result is not an Err");
		}

		int state = 0;
		E err;
	};
}

// now that zpr is seen, we can add printer specialisations
namespace zpr
{
	template <typename T>
	struct print_formatter<nabs::Ok<T>, std::enable_if_t<detail::has_formatter_v<T>>>
	{
		template <typename Cb>
		void print(const nabs::Ok<T>& ok, Cb&& cb, format_args args)
		{
			cb("Ok(");
			detail::print_one(cb, std::move(args), ok.m_value);
			cb(")");
		}
	};

	template <typename E>
	struct print_formatter<nabs::Err<E>, std::enable_if_t<detail::has_formatter_v<E>>>
	{
		template <typename Cb>
		void print(const nabs::Err<E>& err, Cb&& cb, format_args args)
		{
			cb("Err(");
			detail::print_one(cb, std::move(args), err.m_error);
			cb(")");
		}
	};

	template <typename E, typename T>
	struct print_formatter<nabs::Result<T, E>, std::enable_if_t<detail::has_formatter_v<T> && detail::has_formatter_v<E>>>
	{
		template <typename Cb>
		void print(const nabs::Result<T, E>& res, Cb&& cb, format_args args)
		{
			if(res.ok())
			{
				cb("Ok(");
				detail::print_one(cb, std::move(args), res.unwrap());
				cb(")");
			}
			else
			{
				cb("Err(");
				detail::print_one(cb, std::move(args), res.error());
				cb(")");
			}
		}
	};
}


/*
	zmt is included here to maintain a single-header strategy.
	it is available from https://github.com/zhiayang/ztl

	the code is included verbatim, but documentation has been removed.

	zmt.h
	Copyright 2021, zhiayang

	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License.

	Version 0.1.0
	=============
*/

#pragma once

#include <cstdlib>

#include <mutex>
#include <deque>
#include <thread>
#include <atomic>
#include <memory>
#include <functional>
#include <type_traits>
#include <shared_mutex>

// primitives
namespace zmt
{
	// taken from "Move-only version of std::function"
	// https://stackoverflow.com/a/52358928/
	template <typename T>
	struct unique_function : private std::function<T>
	{
	private:
		template <typename Fn, typename = void>
		struct wrapper { };

		// specialise for copyable types
		template <typename Fn>
		struct wrapper<Fn, std::enable_if_t<std::is_copy_constructible_v<Fn>>>
		{
			Fn fn;

			template <typename... Args>
			auto operator() (Args&&... args) { return this->fn(static_cast<Args&&>(args)...); }
		};

		// specialise for move-only types
		template <typename Fn>
		struct wrapper<Fn, std::enable_if_t<!std::is_copy_constructible_v<Fn> && std::is_move_constructible_v<Fn>>>
		{
			Fn fn;

			wrapper(Fn&& fn) : fn(static_cast<Fn&&>(fn)) { }

			wrapper(wrapper&&) = default;
			wrapper& operator= (wrapper&&) = default;

			// in theory, these two functions are never called.
			wrapper(const wrapper& other) : fn(const_cast<Fn&&>(other.fn)) { abort(); }
			wrapper& operator= (const wrapper&) { abort(); }

			template <typename... Args>
			auto operator() (Args&&... args) { return this->fn(static_cast<Args&&>(args)...); }
		};

		using base_type = std::function<T>;

	public:
		unique_function() = default;
		unique_function(std::nullptr_t) : base_type(nullptr) { }

		unique_function(unique_function&&) = default;
		unique_function& operator= (unique_function&&) = default;

		template <typename Fn>
		unique_function(Fn&& fn) : base_type(wrapper<Fn>(static_cast<Fn&&>(fn))) { }

		template <typename Fn>
		unique_function& operator= (Fn&& fn) { base_type::operator=(wrapper<Fn>(static_cast<Fn&&>(fn))); return *this; }

		unique_function& operator= (std::nullptr_t) { base_type::operator=(nullptr); return *this; }

		using base_type::operator();
	};

	template <typename T>
	struct condvar
	{
		condvar() : value() { }
		condvar(const T& x) : value(x) { }
		condvar(T&& x) : value(static_cast<T&&>(x)) { }

		condvar(const condvar&) = delete;
		condvar& operator= (const condvar&) = delete;

		condvar(condvar&&) = default;
		condvar& operator= (condvar&&) = default;

		void set(const T& x)
		{
			this->set_quiet(x);
			this->notify_all();
		}

		void set_quiet(const T& x)
		{
			auto lk = std::lock_guard<std::mutex>(this->mtx);
			this->value = x;
		}

		T get()
		{
			return this->value;
		}

		bool wait(const T& x)
		{
			auto lk = std::unique_lock<std::mutex>(this->mtx);
			this->cv.wait(lk, [&]{ return this->value == x; });
			return true;
		}

		// returns true only if the value was set; if we timed out, it returns false.
		bool wait(const T& x, std::chrono::nanoseconds timeout)
		{
			auto lk = std::unique_lock<std::mutex>(this->mtx);
			return this->cv.wait_for(lk, timeout, [&]{ return this->value == x; });
		}

		template <typename Predicate>
		bool wait_pred(Predicate p)
		{
			auto lk = std::unique_lock<std::mutex>(this->mtx);
			this->cv.wait(lk, p);
			return true;
		}

		// returns true only if the value was set; if we timed out, it returns false.
		template <typename Predicate>
		bool wait_pred(std::chrono::nanoseconds timeout, Predicate p)
		{
			auto lk = std::unique_lock<std::mutex>(this->mtx);
			return this->cv.wait_for(lk, timeout, p);
		}

		void notify_one() { this->cv.notify_one(); }
		void notify_all() { this->cv.notify_all(); }

	private:
		T value;
		std::mutex mtx;
		std::condition_variable cv;

		friend struct semaphore;
	};

	struct semaphore
	{
		semaphore(uint64_t x) : value(x) { }

		void post(uint64_t num = 1)
		{
			{
				auto lk = std::unique_lock<std::mutex>(this->mtx);
				this->value += num;
			}

			if(num > 1) this->cv.notify_all();
			else        this->cv.notify_one();
		}

		void wait()
		{
			auto lk = std::unique_lock<std::mutex>(this->mtx);
			while(this->value == 0)
				this->cv.wait(lk);

			this->value -= 1;
		}

	private:
		uint64_t value = 0;
		std::condition_variable cv;
		std::mutex mtx;
	};




	template <typename T>
	struct wait_queue
	{
		void push(const T& x)
		{
			{
				auto lk = std::unique_lock<std::mutex>(this->mtx);
				this->queue.push_back(x);
			}
			this->sem.post();
		}

		void push(T&& x)
		{
			{
				auto lk = std::unique_lock<std::mutex>(this->mtx);
				this->queue.push_back(static_cast<T&&>(x));
			}
			this->sem.post();
		}

		void push_quiet(const T& x)
		{
			{
				auto lk = std::unique_lock<std::mutex>(this->mtx);
				this->queue.push_back(x);
			}
			this->pending_notifies++;
		}

		void push_quiet(T&& x)
		{
			{
				auto lk = std::unique_lock<std::mutex>(this->mtx);
				this->queue.push_back(static_cast<T&&>(x));
			}
			this->pending_notifies++;
		}


		template <typename... Args>
		void emplace(Args&&... xs)
		{
			{
				auto lk = std::unique_lock<std::mutex>(this->mtx);
				this->queue.emplace_back(static_cast<Args&&>(xs)...);
			}
			this->sem.post();
		}

		template <typename... Args>
		void emplace_quiet(Args&&... xs)
		{
			{
				auto lk = std::unique_lock<std::mutex>(this->mtx);
				this->queue.emplace_back(static_cast<Args&&>(xs)...);
			}
			this->pending_notifies++;
		}

		void notify_pending()
		{
			auto tmp = this->pending_notifies.exchange(0);
			this->sem.post(tmp);
		}

		T pop()
		{
			this->sem.wait();

			{
				auto lk = std::unique_lock<std::mutex>(this->mtx);
				auto ret = static_cast<T&&>(this->queue.front());
				this->queue.pop_front();

				return ret;
			}
		}

		size_t size() const
		{
			return this->queue.size();
		}

		wait_queue() : sem(0) { }

		wait_queue(const wait_queue&) = delete;
		wait_queue& operator= (const wait_queue&) = delete;

		wait_queue(wait_queue&&) = default;
		wait_queue& operator= (wait_queue&&) = default;

	private:
		std::atomic<int64_t> pending_notifies = 0;
		std::deque<T> queue;
		std::mutex mtx;     // mtx is for protecting the queue during push/pop
		semaphore sem;      // sem is for signalling when the queue has stuff (or not)
	};
}

// Synchronised<T>
namespace zmt
{
	template <typename T>
	struct Synchronised
	{
	private:
		struct ReadLockedInstance;
		struct WriteLockedInstance;

		using Lk = std::shared_mutex;

		T value;
		mutable Lk lk;
		std::function<void ()> write_lock_callback = { };

	public:
		Synchronised() { }
		~Synchronised() { }

		Synchronised(const T& x) : value(x) { }
		Synchronised(T&& x) : value(std::move(x)) { }

		template <typename... Args>
		Synchronised(Args&&... xs) : value(std::forward<Args>(xs)...) { }

		Synchronised(Synchronised&&) = delete;
		Synchronised(const Synchronised&) = delete;
		Synchronised& operator= (Synchronised&&) = delete;
		Synchronised& operator= (const Synchronised&) = delete;

		void on_write_lock(std::function<void ()> fn)
		{
			this->write_lock_callback = std::move(fn);
		}

		template <typename Functor>
		void perform_read(Functor&& fn) const
		{
			std::shared_lock lk(this->lk);
			fn(this->value);
		}

		template <typename Functor>
		void perform_write(Functor&& fn)
		{
			if(this->write_lock_callback)
				this->write_lock_callback();

			std::unique_lock lk(this->lk);
			fn(this->value);
		}

		template <typename Functor>
		auto map_read(Functor&& fn) const -> decltype(fn(this->value))
		{
			std::shared_lock lk(this->lk);
			return fn(this->value);
		}

		template <typename Functor>
		auto map_write(Functor&& fn) -> decltype(fn(this->value))
		{
			if(this->write_lock_callback)
				this->write_lock_callback();

			std::unique_lock lk(this->lk);
			return fn(this->value);
		}

		Lk& getLock()
		{
			return this->lk;
		}

		ReadLockedInstance rlock() const
		{
			return ReadLockedInstance(*this);
		}

		WriteLockedInstance wlock()
		{
			if(this->write_lock_callback)
				this->write_lock_callback();

			return WriteLockedInstance(*this);
		}

	private:

		// static Lk& assert_not_held(Lk& lk) { if(lk.held()) assert(!"cannot move held Synchronised"); return lk; }

		struct ReadLockedInstance
		{
			const T* operator -> () { return &this->sync.value; }
			const T* get() { return &this->sync.value; }
			~ReadLockedInstance() { this->sync.lk.unlock_shared(); }

		private:
			ReadLockedInstance(const Synchronised& sync) : sync(sync) { this->sync.lk.lock_shared(); }

			ReadLockedInstance(ReadLockedInstance&&) = delete;
			ReadLockedInstance(const ReadLockedInstance&) = delete;

			ReadLockedInstance& operator = (ReadLockedInstance&&) = delete;
			ReadLockedInstance& operator = (const ReadLockedInstance&) = delete;

			const Synchronised& sync;

			friend struct Synchronised;
		};

		struct WriteLockedInstance
		{
			T* operator -> () { return &this->sync.value; }
			T* get() { return &this->sync.value; }
			~WriteLockedInstance() { this->sync.lk.unlock(); }

		private:
			WriteLockedInstance(Synchronised& sync) : sync(sync) { this->sync.lk.lock(); }

			WriteLockedInstance(WriteLockedInstance&&) = delete;
			WriteLockedInstance(const WriteLockedInstance&) = delete;

			WriteLockedInstance& operator = (WriteLockedInstance&&) = delete;
			WriteLockedInstance& operator = (const WriteLockedInstance&) = delete;

			Synchronised& sync;

			friend struct Synchronised;
		};
	};
}

// async operations (threadpool, futures)
namespace zmt
{
	// workaround a gcc bug: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=85282
	namespace impl
	{
		template <typename E>
		struct future_internals
		{
			future_internals() { cv.set(false); }

			E value;
			condvar<bool> cv;
			bool discard = false;

			future_internals(future_internals&& f) = delete;
			future_internals& operator = (future_internals&& f) = delete;
		};

		template <>
		struct future_internals<void>
		{
			future_internals() { discard = false; cv.set(false); }

			condvar<bool> cv;
			bool discard;

			future_internals(future_internals&& f) = delete;
			future_internals& operator = (future_internals&& f) = delete;
		};
	}


	template <typename T>
	struct future
	{
		template <typename F = T>
		std::enable_if_t<!std::is_same_v<void, F>, T>& get()
		{
			this->wait();
			return this->state->value;
		}

		template <typename F = T>
		void set(std::enable_if_t<!std::is_same_v<void, F>, T>&& x)
		{
			this->state->value = x;
			this->state->cv.set(true);
		}

		template <typename F = T>
		std::enable_if_t<std::is_same_v<void, F>, void> get()
		{
			this->wait();
		}

		template <typename F = T>
		std::enable_if_t<std::is_same_v<void, F>, void> set()
		{
			this->state->cv.set(true);
		}

		void wait() const
		{
			this->state->cv.wait(true);
		}

		void discard()
		{
			this->state->discard = true;
		}

		~future() { if(this->state && !this->state->discard) { this->state->cv.wait(true); } }

		future() { this->state = std::make_shared<impl::future_internals<T>>(); this->state->cv.set(false); }

		template <typename F = T>
		future(std::enable_if_t<!std::is_same_v<void, F>, T>&& val)
		{
			this->state = std::make_shared<impl::future_internals<T>>();
			this->state->value = val;
			this->state->cv.set(true);
		}

		future(future&& f)
		{
			this->state = f.state;
			f.state = nullptr;
		}

		future& operator = (future&& f)
		{
			if(this != &f)
			{
				this->state = f.state;
				f.state = nullptr;
			}

			return *this;
		}

		future(const future&) = delete;
		future& operator = (const future&) = delete;

	private:
		friend struct ThreadPool;

		future(std::shared_ptr<impl::future_internals<T>> st) : state(st) { }
		future clone() { return future(this->state); }

		std::shared_ptr<impl::future_internals<T>> state;
	};

	struct ThreadPool
	{
		template <typename Fn, typename... Args>
		auto run(Fn&& fn, Args&&... args) -> future<decltype(fn(static_cast<Args&&>(args)...))>
		{
			using T = decltype(fn(static_cast<Args&&>(args)...));

			auto fut = future<T>();
			this->jobs.emplace([fn = std::move(fn), args..., f1 = fut.clone()]() mutable {
				if constexpr (!std::is_same_v<T, void>)
				{
					f1.set(fn(static_cast<decltype(args)&&>(args)...));
				}
				else
				{
					fn(static_cast<decltype(args)&&>(args)...);
					f1.set();
				}
			});

			return fut;
		}

		ThreadPool(size_t num = std::thread::hardware_concurrency())
		{
			if(num == 0)
				num = 1;

			this->num_workers = num;
			this->workers = new std::thread[this->num_workers];
			this->start_workers();
		}

		~ThreadPool()
		{
			this->stop_all();
			delete[] this->workers;
		}

		void stop_all()
		{
			this->jobs.push(Job::stop());
			for(size_t i = 0; i < this->num_workers; i++)
				this->workers[i].join();
		}

		void set_max_workers(size_t num)
		{
			this->stop_all();
			delete[] this->workers;

			if(num == 0)
				num = 1;

			this->num_workers = num;
			this->workers = new std::thread[this->num_workers];
			this->start_workers();
		}

		ThreadPool(ThreadPool&&) = delete;
		ThreadPool(const ThreadPool&) = delete;
		ThreadPool& operator = (ThreadPool&&) = delete;
		ThreadPool& operator = (const ThreadPool&) = delete;

	private:
		void start_workers()
		{
			for(size_t i = 0; i < num_workers; i++)
			{
				this->workers[i] = std::thread([this]() {
					worker(this);
				});
			}
		}


		static void worker(ThreadPool* tp)
		{
			while(true)
			{
				auto job = tp->jobs.pop();
				if(job.should_stop)
				{
					tp->jobs.push(Job::stop());
					break;
				}

				job.func();
			}
		}

		struct Job
		{
			bool should_stop = false;
			unique_function<void (void)> func;

			Job() { }
			explicit Job(unique_function<void (void)>&& f) : func(std::move(f)) { }

			static inline Job stop() { Job j; j.should_stop = true; return j; }
		};

		size_t num_workers = 0;
		std::thread* workers = nullptr;

		wait_queue<Job> jobs;
	};

	namespace futures
	{
		template <typename... Args>
		inline void wait(future<Args>&... futures)
		{
			// i love c++17
			(futures.wait(), ...);
		}

		template <typename L>
		inline void wait(const L& futures)
		{
			for(const auto& f : futures)
				f.wait();
		}
	}
}










#include <map>
#include <set>
#include <string>
#include <vector>
#include <optional>
#include <functional>
#include <filesystem>
#include <type_traits>
#include <unordered_map>

#include <cstdio>
#include <cerrno>
#include <cassert>
#include <cstring>
#include <cstdlib>

#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#if defined(_WIN32)

#define WIN32_LEAN_AND_MEAN 1
#define NOMINMAX            1
#include <windows.h>
#include <io.h>

static constexpr int STDIN_FILENO   = 0;
static constexpr int STDOUT_FILENO  = 1;
static constexpr int STDERR_FILENO  = 2;

using ssize_t = long long;

#else
#include <unistd.h>
#include <pthread.h>
#include <sys/wait.h>
#endif // _WIN32


#define NABS_DO_EXPAND(VAL)  VAL ## 1
#define NABS_EXPAND(VAL)     NABS_DO_EXPAND(VAL)

#if !defined(NABS_DECLARATION_ONLY)
	#define NABS_DECLARATION_ONLY 0
#elif (NABS_EXPAND(NABS_DECLARATION_ONLY) == 1)
	#undef NABS_DECLARATION_ONLY
	#define NABS_DECLARATION_ONLY 1
#endif

#if !defined(NABS_NO_COLOURS)
	#define NABS_NO_COLOURS 0
#elif (NABS_EXPAND(NABS_NO_COLOURS) == 1)
	#undef NABS_NO_COLOURS
	#define NABS_NO_COLOURS 1
#endif

#if !defined(NABS_STRICT_COMPILER_CHECK)
	#define NABS_STRICT_COMPILER_CHECK 0
#elif (NABS_EXPAND(NABS_STRICT_COMPILER_CHECK) == 1)
	#undef NABS_STRICT_COMPILER_CHECK
	#define NABS_STRICT_COMPILER_CHECK 1
#endif



// standard stuff for everybody
namespace nabs
{
	// logging
	namespace impl
	{
	#if !NABS_NO_COLOURS
		static constexpr const char* COLOUR_LOG = "\x1b[30;1m";
		static constexpr const char* COLOUR_WARN = "\x1b[1m\x1b[33m";
		static constexpr const char* COLOUR_ERROR = "\x1b[1m\x1b[31m";
		static constexpr const char* COLOUR_RESET = "\x1b[0m";
		static constexpr const char* COLOUR_BOLD  = "\x1b[37m";
	#else
		static constexpr const char* COLOUR_LOG = "";
		static constexpr const char* COLOUR_WARN = "";
		static constexpr const char* COLOUR_ERROR = "";
		static constexpr const char* COLOUR_RESET = "";
		static constexpr const char* COLOUR_BOLD  = "";
	#endif

		template <typename... Args>
		void __logger(int level, std::string_view fmt, Args&&... args)
		{
			const char* colour = COLOUR_ERROR;
			const char* reset = COLOUR_RESET;
			const char* bold = COLOUR_BOLD;
			const char* heading = "[???]";
			const char* heading2 = "";
			const char* heading3 = "";

			if(level == -2)      colour = COLOUR_ERROR, heading = "[err", heading2 = " (internal)", heading3 = "]";
			else if(level == -1) colour = COLOUR_WARN, heading = "[wrn", heading2 = " (internal)", heading3 = "]";
			else if(level == 0)  colour = COLOUR_LOG, heading = "[log]";
			else if(level == 1)  colour = COLOUR_WARN, heading = "[wrn]";
			else if(level == 2)  colour = COLOUR_ERROR, heading = "[err]";

			// if we're not printing to a tty, don't output colours. don't be
			// "one of those" programs.
		#if defined(_WIN32)
			if(GetFileType(GetStdHandle(level > 0 ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE)) != FILE_TYPE_CHAR)
				colour = "", reset = "", bold = "";
		#else
			if(!isatty(level > 0 ? STDERR_FILENO : STDOUT_FILENO))
				colour = "", reset = "", bold = "";
		#endif

			auto output = (level > 0 ? stderr : stdout);
			zpr::fprintln(output, "{}{}{}{}{}{}{}{}{} {}", colour, heading, reset, bold,
				heading2, reset, colour, heading3, reset,
				zpr::sprint(fmt, static_cast<Args&&>(args)...));
		}
	}

	template <typename... Args>
	void log(std::string_view fmt, Args&&... args)
	{
		impl::__logger(0, fmt, static_cast<Args&&>(args)...);
	}

	template <typename... Args>
	void warn(std::string_view fmt, Args&&... args)
	{
		impl::__logger(1, fmt, static_cast<Args&&>(args)...);
	}

	template <typename... Args>
	void error(std::string_view fmt, Args&&... args)
	{
		impl::__logger(2, fmt, static_cast<Args&&>(args)...);
	}

	template <typename... Args>
	[[noreturn]] void error_and_exit(int code, std::string_view fmt, Args&&... args)
	{
		impl::__logger(2, fmt, static_cast<Args&&>(args)...);
		exit(code);
	}

	template <typename... Args>
	[[noreturn]] void error_and_exit(std::string_view fmt, Args&&... args)
	{
		impl::__logger(2, fmt, static_cast<Args&&>(args)...);
		exit(1);
	}

	namespace fs
	{
		using namespace std::filesystem;

		Result<FILE*, std::string> fopen_wrapper(const fs::path& file, const char* mode);
		Result<std::string, std::string> read_file(const fs::path& file);
	}

	namespace impl
	{
		template <typename... Args>
		[[noreturn]] void int_error(Args&&... args)
		{
			impl::__logger(-2, static_cast<Args&&>(args)...);
			exit(69);
		}

		template <typename... Args>
		void int_warn(Args&&... args)
		{
			impl::__logger(-1, static_cast<Args&&>(args)...);
		}
	}

	namespace os
	{
		struct FileOpenFlags
		{
			bool _need_write = false;
			bool _should_create = false;
			bool _append_mode = false;
			bool _truncate_mode = false;
			int _create_perms = 0664;

			inline FileOpenFlags& needs_write(bool x)   { _need_write = x; return *this; }
			inline FileOpenFlags& should_create(bool x) { _should_create = x; return *this; }
			inline FileOpenFlags& append_mode(bool x)   { _append_mode = x; return *this; }
			inline FileOpenFlags& truncate_mode(bool x) { _truncate_mode = x; return *this; }
			inline FileOpenFlags& create_perms(int x)   { _create_perms = x; return *this; }
		};

		std::string get_environment_var(const std::string& name);
	}

	struct Library
	{
		std::string name;
		std::string version;

		std::vector<fs::path> include_paths;
		std::vector<std::string> compiler_flags;
		std::vector<fs::path> additional_compiler_inputs;

		std::vector<fs::path> library_paths;
		std::vector<std::string> libraries;
		std::vector<std::string> linker_flags;
		std::vector<fs::path> additional_linker_inputs;
	};

	/*
		A list of languages supported by nabs. This is not an enumeration because we want
		to make this user-extendable... probably.
	*/
	inline constexpr int LANGUAGE_C         = 1;
	inline constexpr int LANGUAGE_CPP       = 2;
	inline constexpr int LANGUAGE_OBJC      = 3;
	inline constexpr int LANGUAGE_OBJCPP    = 4;

	/*
		Encapsulates a particular binary as a compiler. This can actually represent a compiler, a linker,
		an assembler, objcopy, whatever you want.
	*/
	struct Compiler
	{
		// the path to the compiler binary. if you used find_*_compiler or find_toolchain, then this should exist.
		fs::path path;

		// the 'kind' of the compiler, which (for now) is either clang, gcc, or msvc's cl.exe
		int kind;

		// the language of the compiler, which (for now) is either C or C++.
		int lang;

		// a log hook that you can set to perform things (mostly logging) before the compiler is invoked.
		// you can use default_compiler_logger for a default logging style, although compilers returned by
		// find_toolchain already have this setup appropriately.
		std::function<void (const fs::path&, const std::vector<fs::path>&, const std::vector<std::string>&)> log_hook;

		static constexpr int KIND_UNKNOWN   = 0;
		static constexpr int KIND_CLANG     = 1;
		static constexpr int KIND_GCC       = 2;
		static constexpr int KIND_MSVC_CL   = 3;
		static constexpr int KIND_MSVC_LINK = 4;
	};

	/*
		Encapsulates a set of flags that you pass to a compiler-like binary. Some of the fields
		only make sense for C/C++ compilers (eg. include flags, language standard), but you can
		just use the `options` field to pass whatever flags you want.
	*/
	struct CompilerFlags
	{
		// additional options to pass to the compiler; these are not checked, and are passed as-is.
		std::vector<std::string> options;

		// list of additional include paths (obviously, don't include the '-I', it's just a path)
		std::vector<fs::path> include_paths;

		// names of libraries to pass to the compiler; they are passed as
		// -lfoo for `foo` (on gcc/clang), so don't include the `-l` yourself
		std::vector<std::string> libraries;

		// additional search paths for libraries, these are passed via `-L` on gcc
		// and /link /LIBPATH on msvc. Again, don't include the -L yourself (it's just the path)
		std::vector<fs::path> library_paths;

		// list of defines (passed via -D for gcc, /D for msvc). If the value is empty,
		// it is equivalent to passing -DFOO without any '='.
		std::map<std::string, std::string> defines;

		// folder for the intermediate files (.o, .d, objs, pdbs, that stuff)
		// if this is empty, they'll be generated next to the source file.
		// intermediate files will never appear in the "current directory"
		// (ie. wherever you're running `nabs` from)
		fs::path intermediate_output_folder;

		// list of forced-includes (as if via `-include <filename>` or `/FI`)
		std::vector<fs::path> forced_includes;

		// a list of additional input files to pass to the compiler. these are passed
		// *before* the "normal" input files, for every invocation.
		std::vector<fs::path> pre_additional_input_files;

		// a list of additional input files, but these are passed *after* the normal input files.
		std::vector<fs::path> post_additional_input_files;

		// create any missing folders in any output file paths, as if by `mkdir -p`
		bool create_missing_folders = false;

		// the language standard. this is a string because i can't be bothered right now
		// (also because there's like c++latest on msvc, 0x/1y/1z/2a on gcc/clang)
		std::string language_standard;

		// generate header dependencies using -MMD (gcc/clang) or /showIncludes (msvc cl.exe)
		bool generate_header_dependencies = false;

		// a header to use as a precompiled header (optional)
		fs::path precompiled_header;

		// frameworks, passed via `-framework`. This is only applicable for Clang on macOS/apple targets,
		// and nowhere else.
		std::vector<std::string> frameworks;

		// whether exceptions are disabled. for compatibility with "normal" c++ projects, this is false by default.
		bool exceptions_disabled = false;

		// only applies if the compiler/linker is msvc 'cl.exe' or 'link.exe'. specify whether or not to append
		// .exe to the output name if it did not end in '.exe'. Windows (apparently) has trouble executing files
		// if they don't end in '.exe', so this is TRUE by default.
		bool msvc_ensure_exe_extension = true;

		// only applicable for gcc/clang, and only on linux (but we still pass it, because it's apparently a
		// "well-established" method, even if the OS doesn't require it)
		bool need_pthreads = false;


		// these are internal things -- don't set them directly!

		// a list of all precompiled headers; this is only used by `ldflags`, because of msvc.
		std::vector<fs::path> _precompiled_headers_for_linker;
	};

	/*
		This structure simply encapsulates a set of compilers, so you can pass this around easily.
		You can use find_toolchain() to get a Toolchain struct with sane defaults for your current
		environment, if a sane default exists.
	*/
	struct Toolchain
	{
		Compiler cc;
		CompilerFlags cflags;

		Compiler cxx;
		CompilerFlags cxxflags;

		Compiler objcc;
		CompilerFlags objcflags;

		Compiler objcxx;
		CompilerFlags objcxxflags;

		Compiler ld;
		CompilerFlags ldflags;


		// helper methods
		Toolchain& add_c_flags(const std::vector<std::string>& flags);
		Toolchain& add_cpp_flags(const std::vector<std::string>& flags);
		Toolchain& add_link_flags(const std::vector<std::string>& flags);

		Toolchain& add_c_includes(const std::vector<fs::path>& includes);
		Toolchain& add_cpp_includes(const std::vector<fs::path>& includes);

		Toolchain& use_c_precompiled_header(const fs::path& header);
		Toolchain& use_cpp_precompiled_header(const fs::path& header);

		Toolchain& define(const std::string& name, const std::string& value = "");
		Toolchain& define_c(const std::string& name, const std::string& value = "");
		Toolchain& define_cpp(const std::string& name, const std::string& value = "");

		Toolchain& use_threads(bool use);
		Toolchain& use_exceptions(bool use);

		Toolchain& set_c_standard(const std::string& std);
		Toolchain& set_cpp_standard(const std::string& std);

		template <typename... Flags> Toolchain& add_c_flags(Flags&&... flags);
		template <typename... Flags> Toolchain& add_cpp_flags(Flags&&... flags);
		template <typename... Flags> Toolchain& add_link_flags(Flags&&... flags);

		template <typename... Includes> Toolchain& add_c_includes(Includes&&... includes);
		template <typename... Includes> Toolchain& add_cpp_includes(Includes&&... includes);
	};

	struct LibraryFinderOptions
	{
		// use this to define your own search algorithm. this is always used if it is set.
		// this is the first field to allow initialising with `{ your_finder_fn }` without using
		// designated initialisers.
		std::function<Result<Library, std::string>(const Compiler&, const std::string&, const LibraryFinderOptions&)> custom_searcher;

		bool prefer_static_library = false;
		bool require_static_library = false;
		std::vector<fs::path> additional_search_paths = { };

		// this is contingent on pkg-config being available (also works on windows)
		bool use_pkg_config = true;

		/*
			whether or not to try the "alternate" library folder layout. when searching for things in `additional_search_paths`,
			we try to look for /lib/libNAME.a, etc. however, to be flexible there is this alternate layout, which, if enabled,
			will additionally look for: /NAME/<extra_prefix>/lib/libNAME.a.

			For example, if you have /home/anon/external_libs/llvm/11.0.0/Release/lib/libLLVM.a, you would add
			'/home/anon/external_libs' to the additional search folders, and use '11.0.0/Release' as the extra prefix.
		*/
		bool use_alternate_layout = false;
		std::string alternate_layout_extra_prefix = { };

		// this is only used on unix (ie. not windows)
		bool use_usr = true;
		bool use_usr_local = true;
	};


	/*
		Returns a copy of `s` with leading and trailing whitespace (\t, \n, ' ', \v, \f, \r)
		removed. Also has a string_view version.
	*/
	std::string trim_whitespace(std::string s);
	std::string_view trim_whitespace(std::string_view s);

	/*
		Split a given string into lines. This handles both the dumb windows \r\n line ending,
		and the sane \n Unix line ending.
	*/
	std::vector<std::string_view> split_string_lines(std::string& s);

	/*
		Split a string by the given delimiter. You cannot perfectly emulate split_string_lines
		by using '\n' as the delimiter (thanks, windows...), so don't do it.
	*/
	std::vector<std::string_view> split_string(std::string& s, char delim);

	/*
		Split a string by any whitespace (including \r, \n, \t, ' '). This is useful to emulate
		shell-isms (or make-isms) that split arguments like that.
	*/
	std::vector<std::string_view> split_string_whitespace(std::string& s);

	/*
		similar to std::stoi, but instead of throwing an exception (seriously...) it returns
		an optional.
	*/
	std::optional<int64_t> stoi(const std::string& s, int base = 10);
	std::optional<int64_t> stoi(std::string_view s, int base = 10);


	/*
		Functional map of an iterable container.
	*/
	template <typename... T, template<typename...> class Con, typename Func>
	auto map(const Con<T...>& container, Func&& fn)
	{
		using U = decltype(fn(std::declval<typename Con<T...>::value_type>()));
		if constexpr (std::is_same_v<U, void>)
		{
			for(auto& x : container)
				fn(x);
		}
		else
		{
			Con<U> ret;
			for(auto& x : container)
				ret.push_back(fn(x));

			return ret;
		}
	}

	/*
		Functional filter of an iterable container.
	*/
	template <typename Con, typename Func, typename = std::enable_if_t<
		std::is_same_v<decltype(std::declval<Func>()(std::declval<typename Con::value_type>())), bool>
	>>
	auto filter(const Con& container, Func&& fn)
	{
		Con ret;
		for(auto& x : container)
		{
			if(fn(x))
				ret.push_back(x);
		}
		return ret;
	}

	/*
		A helper function that appends to the given vector, because std::vector::insert has an atrocious
		interface that forces you to make a temporary variable.
	*/
	template <typename T>
	std::vector<T>& append_vector(std::vector<T>& vec, const std::vector<T>& items)
	{
		vec.insert(vec.end(), items.begin(), items.end());
		return vec;
	}

	/*
		The same as append_vector, but it takes an rvalue and thus moves items.
	*/
	template <typename T>
	std::vector<T>& append_vector(std::vector<T>& vec, std::vector<T>&& items)
	{
		vec.insert(vec.end(), std::make_move_iterator(items.begin()), std::make_move_iterator(items.end()));
		return vec;
	}

	/*
		The same as append_vector, but it takes one item instead of a vector of items.
	*/
	template <typename T, typename T1>
	std::vector<T>& append_vector(std::vector<T>& vec, T1&& item)
	{
		vec.push_back(static_cast<T1&&>(item));
		return vec;
	}

	/*
		A helper method to check if the string `sv` ends with the given `suffix`, since this
		only exists as a method of std::string(_view) in C++20.
	*/
	inline bool ends_with(std::string_view sv, std::string_view suffix)
	{
		return sv.size() >= suffix.size()
			&& suffix == sv.substr(sv.size() - suffix.size(), suffix.size());
	}

	/*
		a very strange function. its sole purpose is to be used as `log_hook` in the Compiler. because we
		cannot easily use string literals in template parameters (pre-C++20), it is taken as a normal
		parameter, and this function returns a lambda.
		 name:   what to print, eg. "cc" or "cxx". Note -- don't pass the c_str() of a temporary in here,
		         because the pointer is captured by value, and you'll have a bad time.

		 output: whether the the printed should be the input or the output. For linking, you probably
		         want to show "link foo.exe" instead of "link a.o b.o" etc, so use `true` in this case.
	*/
	inline auto default_compiler_logger(bool output, const char* name, fs::path base = { })
	{
		return [=](const fs::path& out, const std::vector<fs::path>& in, const std::vector<std::string>& args) mutable {
			(void) args;

			if(base.empty())
				base = fs::weakly_canonical(__FILE__).parent_path();

			auto relative_path = [&base](const auto& p) -> auto {
				return p.lexically_proximate(base);
			};

			if(output)
			{
				nabs::log("{} {}", name, relative_path(out).string());
			}
			else
			{
				nabs::log("{} {#}", name, map(in, [&](const auto& x) {
					return relative_path(x).string();
				}));
			}
		};
	}

	// functions changing the global state
	namespace impl
	{
		struct GlobalState
		{
			bool pkg_config_exists;
			bool pkg_config_checked;

			std::unordered_map<std::string, Library> cached_libraries;

			fs::path cached_windows_sdk_root;

			LibraryFinderOptions default_finder_opts;
		};

		zmt::Synchronised<GlobalState>& global_state();
	}

	inline void set_default_library_finder_options(LibraryFinderOptions opts)
	{
		impl::global_state().wlock()->default_finder_opts = std::move(opts);
	}
}

// argument parsing
namespace nabs
{
	std::vector<std::string_view> parse_args(int argc, char** argv, bool include_first = false);
}

#if !NABS_DECLARATION_ONLY
namespace nabs
{
	std::vector<std::string_view> parse_args(int argc, char** argv, bool include_first)
	{
		std::vector<std::string_view> ret;
		for(size_t i = (include_first ? 0 : 1); i < static_cast<size_t>(argc); i++)
			ret.push_back(std::string_view(argv[i]));

		return ret;
	}
}
#endif




// pipes and commands
namespace nabs
{
	namespace os
	{
	#if defined(_WIN32)

		using Fd = HANDLE;
		static constexpr Fd FD_NONE = nullptr;

		struct Proc
		{
			HANDLE handle;

			bool is_thread;
			DWORD error_code;

			bool operator== (const Proc& p) const
			{
				if(this->is_thread != p.is_thread)
					return false;

				return this->handle == p.handle;
			};

			bool operator!= (const Proc& p) const { return !(*this == p); };

			static inline Proc of_pid(HANDLE pid) { return Proc { pid, false, 0 }; }
			static inline Proc of_tid(HANDLE tid) { return Proc { tid, true, 0 }; }
		};

		static constexpr Proc PROC_NONE = Proc { nullptr, false, 0 };

		static size_t PIPE_BUFFER_SIZE = 16384;

		inline fs::path msvc_windows_sdk();
		inline fs::path msvc_toolchain_libraries();
		inline fs::path msvc_toolchain_binaries();

		LPSTR GetLastErrorAsString();
		LPSTR GetErrorCodeAsString(DWORD error);
	#else
		using Fd = int;
		static constexpr Fd FD_NONE = -1;

		struct Proc
		{
			pid_t pid;

			bool is_thread;
			pthread_t tid;

			int error_code;

			bool operator== (const Proc& p) const
			{
				if(this->is_thread != p.is_thread)
					return false;

				return (this->is_thread
					? this->tid == p.tid
					: this->pid == p.pid
				);
			};

			bool operator!= (const Proc& p) const { return !(*this == p); };

			static inline Proc of_pid(pid_t pid) { return Proc { pid, false, 0, 0 }; }
			static inline Proc of_tid(pthread_t tid) { return Proc { -1, true, tid, 0 }; }
		};

		static constexpr Proc PROC_NONE = Proc { -1, false, 0, 0 };

		void dupe_fd(os::Fd src, os::Fd dst);
	#endif

		const char* strerror_wrapper();

		struct PipeDes
		{
			Fd read_end;
			Fd write_end;
		};

		ssize_t read_file(Fd fd, void* buf, size_t len);
		ssize_t write_file(Fd fd, void* buf, size_t len);

		PipeDes make_pipe();
		os::Fd dupe_fd(os::Fd src);
		int wait_for_pid(Proc proc);

		void close_file(Fd fd);
		Fd open_file(const char* path, FileOpenFlags fof);
	}

	namespace impl
	{
		struct Part;
		struct SplitProgArgs;

		void split_transfer(SplitProgArgs args);
		void fd_transfer(os::Fd in_fd, os::Fd out_fd);

		std::string quote_argument(std::string_view arg);

	#if defined(_WIN32)
		std::string make_argument_array(const std::string& exec_name, const std::vector<std::string>& args);
	#else
		char** make_argument_array(const std::string& exec_name, const std::vector<std::string>& args);
	#endif


		struct Pipeline
		{
			inline Pipeline(std::vector<Part> parts) : parts(std::move(parts)) { }
			inline bool empty() const { return this->parts.empty(); }

			int run(std::string* stdout_capture = nullptr, std::string* stderr_capture = nullptr);
			std::vector<os::Proc> runAsync(std::string* stdout_capture = nullptr, std::string* stderr_capture = nullptr);

			std::vector<Part> parts;

			friend struct Part;
			friend void split_transfer(SplitProgArgs args);

		private:
			std::vector<os::Proc> runAsync(os::Fd in_fd, std::string* stdout_capture = nullptr,
				std::string* stderr_capture = nullptr);
		};

		struct Part
		{
			Part(Part&&) = default;
			Part(const Part&) = default;

			Part& operator= (Part&&) = default;
			Part& operator= (const Part&) = default;

			static constexpr int TYPE_PROC          = 1;
			static constexpr int TYPE_FILE          = 2;
			static constexpr int TYPE_SPLIT         = 3;
			static constexpr int TYPE_STRING_OUT    = 4;

			inline int type() const  { return this->_type; }
			inline int flags() const { return this->_flags; }

			static inline Part of_command(std::string name, std::vector<std::string> args = { })
			{
				return Part(TYPE_PROC, 0, std::move(name), std::move(args));
			}

			static inline Part of_file(std::string name)
			{
				return Part(TYPE_FILE, 0, std::move(name), { });
			}

			static inline Part of_split(std::vector<impl::Pipeline> splits, std::vector<impl::Pipeline*> ptrs)
			{
				auto ret = Part(TYPE_SPLIT, 0, "split", { });
				ret.split_vals = std::move(splits);
				ret.split_ptrs = std::move(ptrs);
				return ret;
			}

			static inline Part of_string(std::string* str)
			{
				auto ret = Part(TYPE_STRING_OUT, 0, "string", { });
				ret.string_out = str;
				return ret;
			}

		private:
			int run(os::Fd in_fd, os::Fd out_fd, os::Fd err_fd);
			os::Proc runAsync(os::Fd in_fd, os::Fd out_fd, os::Fd err_fd);

			inline auto make_args() { return impl::make_argument_array(this->name, this->arguments); }

			inline Part(int type, int flags, std::string name, std::vector<std::string> args)
				: _type(type)
				, _flags(flags)
				, name(std::move(name))
				, arguments(std::move(args))
			{ }

			int _type;
			int _flags;
			std::string name;
			std::string* string_out = 0;

			std::vector<std::string> arguments;

			std::vector<impl::Pipeline> split_vals;
			std::vector<impl::Pipeline*> split_ptrs;

			friend struct Pipeline;
		};

		struct SplitProgArgs
		{
			os::Fd read_fd;
			os::Fd write_fd;
			std::vector<os::Fd> write_fds;

			std::vector<impl::Pipeline> split_vals;
			std::vector<impl::Pipeline*> split_ptrs;
		};

		// ADL should let us find this operator.
		Pipeline operator| (Pipeline head, const Pipeline& tail);
	}


	template <typename... Args>
	inline impl::Pipeline cmd(std::string exe, Args&&... args)
	{
		static_assert(((std::is_convertible_v<Args, std::string>) && ...),
			"arguments to cmd() must be convertible to std::string");

		return impl::Pipeline({
			impl::Part::of_command(std::move(exe), std::vector<std::string>{std::forward<Args>(args)...})
		});
	}

	inline impl::Pipeline cmd(std::string exe, std::vector<std::string> args)
	{
		return impl::Pipeline({
			impl::Part::of_command(std::move(exe), std::move(args))
		});
	}

	inline impl::Pipeline file(fs::path path)
	{
		return impl::Pipeline({
			impl::Part::of_file(path.string())
		});
	}

	inline impl::Pipeline to_string(std::string* str)
	{
		return impl::Pipeline({
			impl::Part::of_string(str)
		});
	}

	namespace impl
	{
		template <typename T>
		inline void split_helper(std::vector<Pipeline>& vals, std::vector<Pipeline*>& ptrs, T&& thing)
		{
			static_assert(std::is_same_v<std::decay_t<T>, Pipeline>, "split() requires pipes");
			static_assert(std::is_rvalue_reference_v<decltype(thing)> || std::is_lvalue_reference_v<decltype(thing)>,
				"split() requires either lvalue or rvalue references as arguments");

			if constexpr (std::is_rvalue_reference_v<decltype(thing)>)
				vals.push_back(std::move(thing));

			else
				ptrs.push_back(&thing);
		}

		template <typename T, typename... Args>
		inline void split_helper(std::vector<Pipeline>& vals, std::vector<Pipeline*>& ptrs, T&& thing, Args&&... things)
		{
			split_helper(vals, ptrs, std::forward<T>(thing));
			split_helper(vals, ptrs, std::forward<Args>(things)...);
		}
	}

	template <typename... Args>
	inline impl::Pipeline split(Args&&... args)
	{
		static_assert(sizeof...(args) > 0, "split() requires at least one argument");

		std::vector<impl::Pipeline> vals;
		std::vector<impl::Pipeline*> ptrs;

		impl::split_helper(vals, ptrs, std::forward<Args>(args)...);

		return impl::Pipeline({
			impl::Part::of_split(std::move(vals), std::move(ptrs))
		});
	}

	template <typename... Args>
	inline impl::Pipeline tee(Args&&... args)
	{
		static_assert(((std::is_convertible_v<Args, fs::path>) && ...),
			"tee() requires std::filesystem::path");

		return impl::Pipeline({
			impl::Part::of_split(std::vector<impl::Pipeline> { nabs::file(args)... }, { })
		});
	}
}

// implementation
#if !NABS_DECLARATION_ONLY
namespace nabs
{
	namespace os
	{
		ssize_t read_file(Fd fd, void* buf, size_t len)
		{
		#if defined(_WIN32)
			DWORD did = 0;

			// annoyingly, this shit returns false and sets error to broken pipe even if bytes could be read.
			if(!ReadFile(fd, buf, static_cast<DWORD>(len), &did, nullptr) && GetLastError() !=  ERROR_BROKEN_PIPE)
			{
				impl::int_warn("ReadFile(): {}", GetLastErrorAsString());
				return -1;
			}
			return static_cast<ssize_t>(did);
		#else
			if(auto ret = read(fd, buf, len); ret >= 0)
				return ret;
			else
				impl::int_error("read(): {}", strerror(errno));
		#endif
		}

		ssize_t write_file(Fd fd, void* buf, size_t len)
		{
		#if defined(_WIN32)
			DWORD did = 0;

			// annoyingly, this shit returns false and sets error to broken pipe even if bytes could be written.
			if(!WriteFile(fd, buf, static_cast<DWORD>(len), &did, nullptr) && GetLastError() !=  ERROR_BROKEN_PIPE)
			{
				impl::int_warn("WriteFile(): {}", GetLastErrorAsString());
				return -1;
			}

			return static_cast<ssize_t>(did);
		#else
			if(auto ret = write(fd, buf, len); ret >= 0)
				return ret;
			else
				impl::int_error("write(): {}", strerror(errno));
		#endif
		}

		PipeDes make_pipe()
		{
		#if defined(_WIN32)
			Fd p_read;
			Fd p_write;
			SECURITY_ATTRIBUTES attr { };
			memset(&attr, 0, sizeof(attr));
			attr.nLength = sizeof(attr);
			attr.bInheritHandle = true;

			if(!CreatePipe(&p_read, &p_write, &attr, 0))
				impl::int_error("CreatePipe(): {}", GetLastErrorAsString());

			return PipeDes { p_read, p_write };
		#else
			if(int p[2]; pipe(p) < 0)
			{
				impl::int_error("pipe(): {}", strerror(errno));
			}
			else
			{
				// set the pipes to close on exec, so that we do not have dangling write ends
				// left open in children.
				if(fcntl(p[0], F_SETFD, FD_CLOEXEC) < 0 || fcntl(p[1], F_SETFD, FD_CLOEXEC) < 0)
					impl::int_error("fcntl(FD_CLOEXEC): {}", strerror(errno));

				return PipeDes {
					.read_end = p[0],
					.write_end = p[1]
				};
			}
		#endif
		}

		void close_file(Fd fd)
		{
		#if defined(_WIN32)
			if(!CloseHandle(fd))
				impl::int_error("CloseHandle(): {}", GetLastErrorAsString());
		#else
			if(close(fd) < 0)
				impl::int_error("close(): {}", strerror(errno));
		#endif
		}

		#if !defined(_WIN32)
		void dupe_fd(os::Fd src, os::Fd dst)
		{
			// dup2 closes dst for us.
			if(dup2(src, dst) < 0)
				impl::int_error("dup2({}, {}): {}", src, dst, strerror(errno));

			os::close_file(src);
		}
		#endif

		os::Fd dupe_fd(os::Fd src)
		{
			if(src == os::FD_NONE)
				return src;

		#if defined(_WIN32)

			auto proc = GetCurrentProcess();
			HANDLE dst { };

			// note: by default don't let this be inheritable...
			if(!DuplicateHandle(proc, src, proc, &dst, 0, false, DUPLICATE_SAME_ACCESS))
				impl::int_error("DuplicateHandle(): {}", GetLastErrorAsString());

			return dst;

		#else
			auto ret = os::FD_NONE;
			if(ret = dup(src); ret < 0)
				impl::int_error("dup({}): {}", src, strerror(errno));

			// on unix, it is important to make this new pipe also cloexec.
			if(fcntl(ret, F_SETFD, FD_CLOEXEC) < 0)
				impl::int_error("fcntl(): {}", os::strerror_wrapper());

			return ret;
		#endif
		}

		Fd open_file(const char* path, FileOpenFlags fof)
		{
			int flags = 0;
			Fd fd = 0;

		#if defined(_WIN32)

			SECURITY_ATTRIBUTES attr { };
			memset(&attr, 0, sizeof(attr));
			attr.nLength = sizeof(attr);
			attr.bInheritHandle = true;

			fd = CreateFile(
				path,
				GENERIC_READ | (fof._need_write ? GENERIC_WRITE : 0),
				FILE_SHARE_READ,
				&attr,
				fof._should_create ? OPEN_ALWAYS : OPEN_EXISTING,
				FILE_ATTRIBUTE_NORMAL,
				nullptr
			);

			if(fd == INVALID_HANDLE_VALUE)
				impl::int_error("CreateFile(): {}", GetLastErrorAsString());

		#else
			if(fof._need_write)         flags |= O_RDWR;
			else                        flags |= O_RDONLY;

			if(fof._should_create)      flags |= O_CREAT;
			if(fof._truncate_mode)      flags |= O_TRUNC;
			else if(fof._append_mode)   flags |= O_APPEND;

			if(fof._should_create)
				fd = open(path, flags, fof._create_perms);
			else
				fd = open(path, flags);

			if(fd < 0)
				impl::int_error("open('{}'): {}", path, strerror(errno));
		#endif

			return fd;
		}

		int wait_for_pid(Proc proc)
		{
		#if defined(_WIN32)
			if(WaitForSingleObject(proc.handle, INFINITE) != WAIT_OBJECT_0)
				impl::int_error("WaitForSingleObject(): {}", GetLastErrorAsString());

			// we are using this for threads as well; if it is not a process, return 0.
			DWORD status = 0;
			if(!proc.is_thread)
			{
				if(!GetExitCodeProcess(proc.handle, &status))
					impl::int_error("GetExitCodeProcess(): {}", GetLastErrorAsString());
			}

			CloseHandle(proc.handle);
			return static_cast<int>(status);
		#else
			if(proc.is_thread)
			{
				return pthread_join(proc.tid, nullptr);
			}
			else
			{
				int status = 0;
			again:
				if(waitpid(proc.pid, &status, 0) < 0)
				{
					if(errno == EINTR)
						goto again;
					impl::int_error("waitpid({}): {}", proc.pid, strerror(errno));
				}

				return status;
			}
		#endif
		}

		const char* strerror_wrapper()
		{
		#if defined(_WIN32)
			static char buf[512] { };
			strerror_s(buf, 512, errno);
			return buf;
		#else
			return strerror(errno);
		#endif
		}
	}

	namespace impl
	{
		#if defined(_WIN32)
			// https://docs.microsoft.com/en-gb/archive/blogs/twistylittlepassagesallalike/everyone-quotes-command-line-arguments-the-wrong-way
			std::string quote_argument(std::string_view s)
			{
				if(s.find_first_of(" \t\n\v\f") == std::string::npos)
					return std::string(s);

				std::string ret = "\"";
				int backs = 0;

				for(auto it = s.begin(); ; ++it)
				{
					int backs = 0;
					while(it != s.end() && *it == '\\')
						++it, ++backs;

					if(it == s.end())
					{
						ret.append(backs * 2, '\\');
						break;
					}
					else if(*it == '"')
					{
						ret.append(backs * 2 + 1, '\\');
						ret.push_back(*it);
					}
					else
					{
						ret.append(backs, '\\');
						ret.push_back(*it);
					}
				}

				ret.push_back('"');
				return ret;
			}

			std::string make_argument_array(const std::string& exec_name, const std::vector<std::string>& args)
			{
				std::string ret = quote_argument(exec_name);
				for(auto& arg : args)
				{
					ret += " ";
					ret += quote_argument(arg);
				}

				return ret;
			}
		#else
			// on unix, char** argv is passed directly, so unless the program is really stupid, there's
			// actually no need to quote space-containing paths.
			std::string quote_argument(std::string_view s)
			{
				return std::string(s);
			}

			char** make_argument_array(const std::string& exec_name, const std::vector<std::string>& args)
			{
				char** args_array = new char*[args.size() + 2];
				for(size_t i = 0; i < args.size(); i++)
					args_array[1 + i] = const_cast<char*>(args[i].c_str());

				args_array[0] = const_cast<char*>(exec_name.c_str());
				args_array[args.size() + 1] = nullptr;
				return args_array;
			}
		#endif // _WIN32

		// ADL should let us find this operator.
		Pipeline operator| (Pipeline head, const Pipeline& tail)
		{
			constexpr auto TYPE_FILE = Part::TYPE_FILE;

			// the size needs to be > 1 -- such that if the back of the pipeline is a file,
			// we know that it has to be the 'output' file; we must still allow an 'input' file.
			if(head.parts.size() > 1 && head.parts.back().type() == TYPE_FILE)
				int_error("file() components cannot appear in the middle of a pipeline");

			// check if we are trying to chain 2 files together.
			if(!head.parts.empty() && !tail.parts.empty()
				&& head.parts.back().type() == TYPE_FILE && tail.parts.front().type() == TYPE_FILE)
				int_error("file() components cannot appear consecutively in a pipeline");

			head.parts.insert(head.parts.end(), tail.parts.begin(), tail.parts.end());
			return head;
		}

		int Part::run(os::Fd in_fd, os::Fd out_fd, os::Fd err_fd)
		{
			auto child_pid = this->runAsync(in_fd, out_fd, err_fd);
			if(child_pid.error_code != 0)
				return static_cast<int>(child_pid.error_code);

			if(child_pid == os::PROC_NONE)
				return 0;

			return os::wait_for_pid(child_pid);
		}

		os::Proc Part::runAsync(os::Fd in_fd, os::Fd out_fd, os::Fd err_fd)
		{
			#if defined(_WIN32)
				using WorkerRetty = DWORD;
			#else
				using WorkerRetty = void*;
			#endif

			/*
				small explanation of `child_close`: on unix, we set the CLOEXEC flag on the
				pipe descriptors, so they automatically close when the child process execs, so it does
				not have an open handle to the write end of its own input pipe.

				this is only relevant for split() pipes, because split() itself spawns a child process
				(only only on unix), but it doesn't exec(). so, if we are not careful, the children
				spawned by the split() (ie. the grandchildren of the main program) will inherit the
				write-end of split()'s input pipe, causing split() to never exit. thus, it needs to be
				closed manually.

				on windows, this is irrelevant, because we always specify exactly the 3 handles (In, Out, Err)
				that we want the child to inherit, so we can safely ignore `child_close` on windows.
			*/
			if(this->type() == TYPE_PROC)
			{
			#if defined(_WIN32)

				auto default_handle = [](os::Fd fd, DWORD def) -> HANDLE {
					return fd == os::FD_NONE ? GetStdHandle(def) : fd;
				};

				STARTUPINFOEX info;
				memset(&info, 0, sizeof(info));

				info.StartupInfo.cb = sizeof(STARTUPINFOEX);
				info.StartupInfo.hStdInput  = default_handle(in_fd, STD_INPUT_HANDLE);
				info.StartupInfo.hStdOutput = default_handle(out_fd, STD_OUTPUT_HANDLE);
				info.StartupInfo.hStdError  = default_handle(err_fd, STD_ERROR_HANDLE);
				info.StartupInfo.dwFlags |= STARTF_USESTDHANDLES;

				std::vector<HANDLE> inherited_handles = {
					info.StartupInfo.hStdInput,
					info.StartupInfo.hStdOutput,
					info.StartupInfo.hStdError
				};

				SIZE_T attrib_size = 0;
				InitializeProcThreadAttributeList(nullptr, 1, 0, &attrib_size);

				auto attrib_buffer = new char[attrib_size];
				auto attrib_list = reinterpret_cast<LPPROC_THREAD_ATTRIBUTE_LIST>(attrib_buffer);

				if(!InitializeProcThreadAttributeList(attrib_list, 1, 0, &attrib_size))
					impl::int_error("InitializeProcThreadAttributeList(): {}", os::GetLastErrorAsString());

				auto succ = UpdateProcThreadAttribute(attrib_list, 0, PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
					inherited_handles.data(), inherited_handles.size() * sizeof(HANDLE),
					nullptr, nullptr);

				if(!succ)
					impl::int_error("UpdateProcThreadAttribute(): {}", os::GetLastErrorAsString());

				info.lpAttributeList = attrib_list;

				PROCESS_INFORMATION procinfo;
				memset(&procinfo, 0, sizeof(procinfo));

				auto cmdline = this->make_args();
				auto cmdline_ = const_cast<LPSTR>(cmdline.c_str());

				auto result = CreateProcessA(
					nullptr,                        // LPCSTR                   lpApplicationName
					cmdline_,                       // LPSTR                    lpCommandLine
					nullptr,                        // LPSECURITY_ATTRIBUTES    lpProcessAttributes
					nullptr,                        // LPSECURITY_ATTRIBUTES    lpThreadAttributes
					true,                           // BOOL                     bInheritHandles
					EXTENDED_STARTUPINFO_PRESENT,   // DWORD                    dwCreationFlags
					nullptr,                        // LPVOID                   lpEnvironment
					nullptr,                        // LPCSTR                   lpCurrentDirectory
					&info.StartupInfo,              // LPSTARTUPINFO            lpStartupInfo
					&procinfo                       // LPPROCESS_INFORMATION    lpProcessInformation
				);

				DeleteProcThreadAttributeList(attrib_list);
				delete[] attrib_buffer;

				if(!result)
				{
					// impl::int_error("CreateProcess('{}'): {}", cmdline, os::GetLastErrorAsString());
					// return Err(static_cast<int>(GetLastError()));
					return os::Proc { 0, false, GetLastError() };
				}
				else
				{
					CloseHandle(procinfo.hThread);
					return os::Proc::of_pid(procinfo.hProcess);
				}
			#else
				if(auto child = fork(); child < 0)
				{
					int_error("fork(): {}", os::strerror_wrapper());
				}
				else if(child == 0)
				{
					if(in_fd != os::FD_NONE)
					{
						os::dupe_fd(in_fd, STDIN_FILENO);
					}
					else
					{
						// TODO(#4): make closing stdin configurable
						// for now, we leave stdin open. this might be a problem later,
						// and we definitely want to make it configurable later on.
					}

					if(out_fd != os::FD_NONE)
						os::dupe_fd(out_fd, STDOUT_FILENO);

					if(err_fd != os::FD_NONE)
						os::dupe_fd(err_fd, STDERR_FILENO);

					if(auto err = execvp(this->name.c_str(), this->make_args()); err < 0)
						exit(errno);

					// we want to return an error to the caller in a nice way instead of terminating everything.
					// int_error("execvp('{}'): {}", this->name, os::strerror_wrapper());
					abort();
				}
				else
				{
					return os::Proc::of_pid(child);
				}
			#endif // _WIN32
			}
			else if(this->type() == TYPE_FILE)
			{
				auto make_the_file = [this, &in_fd]() -> os::Fd {
					// if the in_fd is not -1, then we need to write to the file.
					return os::open_file(this->name.c_str(), os::FileOpenFlags()
						.needs_write(in_fd != os::FD_NONE)

						// TODO(#5): make a way to specify the file opening flags/modes
						.append_mode(true)      // append by default
						.should_create(true)    // create by default
						.create_perms(0664)
					);
				};

				// files cannot be both read and write (use split/tee for that)
				// while this is an "artificial" limitation, i'm going to enforce it for now.
				if(in_fd != os::FD_NONE && out_fd != os::FD_NONE)
					impl::int_error("file() cannot appear in the middle of a pipeline");

				struct Args
				{
					os::Fd in_fd;
					os::Fd out_fd;
				};

				auto worker = [](void* arg) -> WorkerRetty {
					auto fds = reinterpret_cast<Args*>(arg);
					fd_transfer(fds->in_fd, fds->out_fd);
					os::close_file(fds->in_fd);
					os::close_file(fds->out_fd);
					delete fds;
					return 0;
				};

				auto thread_arg = new Args;
				auto file_fd = make_the_file();
				if(in_fd != os::FD_NONE)
				{
					thread_arg->in_fd = os::dupe_fd(in_fd);
					thread_arg->out_fd = file_fd;
				}
				else if(out_fd != os::FD_NONE)
				{
					thread_arg->in_fd = file_fd;
					thread_arg->out_fd = os::dupe_fd(out_fd);
				}
				else
				{
					int_error("unreachable");
				}
			#if defined(_WIN32)
				auto thr = CreateThread(
					nullptr,    // LPSECURITY_ATTRIBUTES    lpThreadAttributes
					0,          // DWORD                    dwStackSize
					worker,     // LPTHREAD_START_ROUTINE   lpStartAddress
					thread_arg, // LPVOID                   lpParameter
					0,          // DWORD                    dwCreationFlags
					nullptr     // LPDWORD                  lpThreadId
				);

				if(thr == nullptr)
					int_error("CreateThread(): {}", os::GetLastErrorAsString());

				return os::Proc::of_tid(thr);
			#else
				pthread_t tid = 0;
				if(pthread_create(&tid, /* attribs: */ nullptr, worker, thread_arg) < 0)
					int_error("pthread_create(): {}", os::strerror_wrapper());

				return os::Proc::of_tid(tid);
			#endif
			}
			else if(this->type() == TYPE_SPLIT)
			{
				if(in_fd == os::FD_NONE)
					int_error("split() cannot be the first item in a pipeline");

				auto worker = [](void* arg) -> WorkerRetty {
					auto args = reinterpret_cast<SplitProgArgs*>(arg);
					split_transfer(*args);
					os::close_file(args->read_fd);
					os::close_file(args->write_fd);

					delete args;
					return 0;
				};

				if(out_fd == os::FD_NONE)
				{
				#if defined(_WIN32)
					out_fd = GetStdHandle(STD_OUTPUT_HANDLE);
				#else
					out_fd = STDOUT_FILENO;
				#endif
				}

				auto thread_arg = new SplitProgArgs;
				thread_arg->read_fd = os::dupe_fd(in_fd);
				thread_arg->write_fd = os::dupe_fd(out_fd);
				thread_arg->split_vals = this->split_vals;
				thread_arg->split_ptrs = this->split_ptrs;
			#if defined(_WIN32)
				auto thr = CreateThread(
					nullptr,    // LPSECURITY_ATTRIBUTES    lpThreadAttributes
					0,          // DWORD                    dwStackSize
					worker,     // LPTHREAD_START_ROUTINE   lpStartAddress
					thread_arg, // LPVOID                   lpParameter
					0,          // DWORD                    dwCreationFlags
					nullptr     // LPDWORD                  lpThreadId
				);

				if(thr == nullptr)
					int_error("CreateThread(): {}", os::GetLastErrorAsString());

				return os::Proc::of_tid(thr);
			#else
				pthread_t tid = 0;
				if(pthread_create(&tid, /* attribs: */ nullptr, worker, thread_arg) < 0)
					int_error("pthread_create(): {}", os::strerror_wrapper());

				return os::Proc::of_tid(tid);
			#endif
			}
			else if(this->type() == TYPE_STRING_OUT)
			{
				if(in_fd == os::FD_NONE)
					int_error("to_string() cannot be the first item in a pipeline");

				struct Arg
				{
					os::Fd read_fd;
					std::string* write_str = 0;
				};

				auto thread_arg = new Arg;
				thread_arg->read_fd = os::dupe_fd(in_fd);
				thread_arg->write_str = this->string_out;

				// because we need to read back to a string in the current (base) process,
				// we cannot use fork; this *must* be a thread.
				auto worker = [](void* _arg) -> WorkerRetty {
					auto arg = reinterpret_cast<Arg*>(_arg);

					char buf[4096] { };
					while(true)
					{
						auto did = os::read_file(arg->read_fd, buf, 4096);
						if(did <= 0) break;

						if(arg->write_str != nullptr)
							arg->write_str->append(buf, did);
					}

					os::close_file(arg->read_fd);
					delete arg;
					return 0;
				};

			#if defined(_WIN32)
				auto thr = CreateThread(
					nullptr,    // LPSECURITY_ATTRIBUTES    lpThreadAttributes
					0,          // DWORD                    dwStackSize
					worker,     // LPTHREAD_START_ROUTINE   lpStartAddress
					thread_arg, // LPVOID                   lpParameter
					0,          // DWORD                    dwCreationFlags
					nullptr     // LPDWORD                  lpThreadId
				);

				if(thr == nullptr)
					int_error("CreateThread(): {}", os::GetLastErrorAsString());

				return os::Proc::of_tid(thr);
			#else
				pthread_t tid = 0;
				if(pthread_create(&tid, /* attribs: */ nullptr, worker, thread_arg) < 0)
					int_error("pthread_create(): {}", os::strerror_wrapper());

				return os::Proc::of_tid(tid);
			#endif
			}
			else
			{
				int_error("unknown pipeline component type '{}'", this->type());
			}
		}

		std::vector<os::Proc> Pipeline::runAsync(os::Fd in_fd, std::string* stdout_capture, std::string* stderr_capture)
		{
			std::vector<os::Proc> children;
			os::Fd predecessor_pipe = in_fd;

			if(this->empty())
				int_error("unexpected empty pipeline");

			for(size_t i = 0; i < this->parts.size(); i++)
			{
				bool is_last = (i == this->parts.size() - 1); // && (stdout_capture == nullptr);
				auto [ pipe_read, pipe_write ] = os::make_pipe();

				auto stderr_write = os::FD_NONE;

				if(is_last)
				{
					// if we need to capture stdout, don't close the write end of this pipe yet.
					if(stdout_capture == nullptr)
					{
						os::close_file(pipe_write);
						pipe_write = os::FD_NONE;
					}

					// if we need to capture stderr, make a new pipe for it, and run the of_string.
					if(stderr_capture != nullptr)
					{
						auto [ r, w ] = os::make_pipe();
						stderr_write = w;

						auto child = Part::of_string(stderr_capture).runAsync(r, os::FD_NONE, os::FD_NONE);
						children.push_back(child);
					}
				}

				/*
					TODO(#6): stderr is (still) not handled in a very nice way.
					right now, we can run an entire pipeline and capture the stderr, but that
					stderr is only for the last process in the chain. everyone else's stderr
					is printed to console, and that might not be very desirable.

					The ideal implementation is to have a Part that can collect stderrs, and
					also have a Part that can merge stdout/stderr and pass it to the next program
					(something like 2&>1)
				*/
				auto child = this->parts[i].runAsync(predecessor_pipe, pipe_write, stderr_write);

				if(child != os::PROC_NONE)
					children.push_back(child);

				// setup the read end for the next process.
				if(predecessor_pipe != os::FD_NONE)
					os::close_file(predecessor_pipe);

				if(stderr_write != os::FD_NONE)
					os::close_file(stderr_write);

				predecessor_pipe = pipe_read;
				if(pipe_write != os::FD_NONE)
					os::close_file(pipe_write);
			}

			if(stdout_capture != nullptr)
			{
				auto child = Part::of_string(stdout_capture).runAsync(predecessor_pipe, os::FD_NONE, os::FD_NONE);
				children.push_back(child);
			}

			os::close_file(predecessor_pipe);
			return children;
		}

		void fd_transfer(os::Fd in_fd, os::Fd out_fd)
		{
			char buf[4096] { };
			while(true)
			{
				auto did = os::read_file(in_fd, buf, 4096);
				if(did <= 0)
					break;

				os::write_file(out_fd, buf, did);
			}
		}

		void split_transfer(SplitProgArgs args)
		{
			std::vector<os::Fd> fds;
			std::vector<os::Proc> children;

			auto iterate_splits = [&fds, &children](Pipeline* pl) {

				assert(pl != nullptr);
				auto [ p_read, p_write ] = os::make_pipe();

				auto cs = pl->runAsync(p_read);
				children.insert(children.end(), cs.begin(), cs.end());
				fds.push_back(p_write);
			};

			for(auto& sp : args.split_ptrs)
				iterate_splits(sp);

			for(auto& sp : args.split_vals)
				iterate_splits(&sp);

			char buf[4096] { };
			while(true)
			{
				auto n = os::read_file(args.read_fd, buf, 4096);
				if(n <= 0)
					break;

				os::write_file(args.write_fd, buf, n);
				for(auto& f : fds)
					os::write_file(f, buf, n);
			}

			for(auto& f : fds)
				os::close_file(f);

			// wait for all the children...
			for(auto c : children)
				os::wait_for_pid(c);
		}

		std::vector<os::Proc> Pipeline::runAsync(std::string* stdout_capture, std::string* stderr_capture)
		{
			return this->runAsync(os::FD_NONE, stdout_capture, stderr_capture);
		}

		int Pipeline::run(std::string* stdout_capture, std::string* stderr_capture)
		{
			auto children = this->runAsync(stdout_capture, stderr_capture);

			int status = 0;
			for(auto c : children)
			{
				int child_st = os::wait_for_pid(c);

				// ignore status codes for threads.
				if(!c.is_thread)
					status = child_st;
			}

			// just use the last one.
			return status;
		}
	}
}
#endif // !NABS_DECLARATION_ONLY



// dependency stuff
namespace nabs::dep
{
	/*
		A dependency item; it can have different 'kinds', which can include source files, object files,
		phony targets, etc. An Item has zero or more dependencies, which are other Items. `Item` instances
		are managed by the `Graph` (see below) -- don't manually create these (although you can't because
		the constructor is private)

		To create a dependency on another item, use the `.depend()` method; for example, to
		make the Item 'a' depend on 'b', do `a->depend(b)`. The `deps` field is public, so it
		is ok to add/remove items from the dependency list (it is managed solely by the Item,
		and there is no magic).

		There is also the `produced_by` field; this is similar to, but distinct from, the dependencies
		of an item. For example, an object file might depend on its C source file, but it might also
		depend on the headers included by the source file (so that it can be rebuilt if they change).
		In this scenario, while the object file depends on all those, it is *produced* solely by
		the source file -- so that should be placed in `produced_by`.

		Finally, there are two fields -- `user_data` and `user_flags` that are used to store user data:
		whatever you want.

		Note that the 'kind' of an item is purely theoretical unless you use any of the auto_compile
		functions. They are only used to determine how to 'produce' the file; for example, an object file
		is produced by calling `compile_to_object_file()` on the `produced_by` item, while an executable
		must be produced by calling `link_object_files()`. Again, if you are running the entire compilation
		process "manually", then the kind of an Item is up to you to use (or not use) and interpret.

		Importantly, the Graph does not differentiate two items with the same name but different kinds; the
		name is the only identifying key.


		TODO(#7): no way to specify optional dependencies
	*/
	struct Item
	{
		// add a dependency from this item to the provided item.
		// If the dependency already exists, it is ignored.
		void depend(Item* item);

		// add an item to the produced_by, *AND* add it to the dependencies as well.
		void add_produced_by(Item* item);

		inline int kind() const { return this->_kind; }
		inline const std::string& name() const { return this->_name; }

		std::vector<Item*> deps;
		std::vector<Item*> produced_by;

		// this is only used if kind == KIND_SYSTEM_LIBRARY i guess
		LibraryFinderOptions lib_finder_options;

		// this is for your own use.
		uint64_t user_flags = 0;
		void* user_data = nullptr;

		std::vector<std::string> dependecies_as_array() const;

		unsigned int flags = 0;

		static constexpr unsigned int FLAG_MIGHT_NOT_EXIST = 0x1;

	private:
		Item() { }
		Item(int kind, std::string name)
			: _kind(kind)
			, _name(std::move(name))
		{ }

		// note: don't set these manually please. helper vars for toposort.
		int _kind = 0;
		std::string _name;
		size_t dependents = 0;

		mutable size_t level = 0;

		friend struct Graph;
	};

	inline constexpr int KIND_NONE           = 0;
	inline constexpr int KIND_C_SOURCE       = 1;
	inline constexpr int KIND_CPP_SOURCE     = 2;
	inline constexpr int KIND_OBJECT         = 3;
	inline constexpr int KIND_EXE            = 4;
	inline constexpr int KIND_PHONY          = 5;
	inline constexpr int KIND_PCH            = 6;
	inline constexpr int KIND_OBJC_SOURCE    = 7;
	inline constexpr int KIND_OBJCPP_SOURCE  = 8;
	inline constexpr int KIND_SYSTEM_LIBRARY = 9;

	/*
		A dependency graph, containing zero or more items. The graph is the owner of items created by it, and
		its items will be destroyed when the graph is destroyed.

		The graph serves two main purposes:
		1. centralised place to store/retrieve Item instances. Items are uniqued by their name (for obvious reasons),
			so there must be a place to associate a name with a particular instance of item.

		2. topologically sorting the known items in dependency order and verifying that there are no circular
			dependencies.

		When adding non-phony items, `std::filesystem::weakly_canonical(path)` is called to mitigate the effect
		of relative paths on the ability to uniquely identify files by path.
	*/
	struct Graph
	{
		~Graph();

		// Add an item with the specified kind to the dependency graph, and return the Item representing it.
		// While it is not an error to call add() on an existing path, a warning will be emitted. In most
		// cases, you should prefer using get_or_add() instead.
		Item* add(int kind, std::string name);
		Item* add(int kind, const char* name);

		// add a file with the given path as an item; fs::weakly_canonical() is called on the given path.
		Item* add(int kind, const fs::path& path);

		// add a file, but infer the kind from the filename
		Item* add(const fs::path& path);

		// add a library. assumes the kind is KIND_SYSTEM_LIBRARY
		Item* add_system_library(std::string name, std::optional<LibraryFinderOptions> opts = { });

		// Get the item with the given name. For items representing files, the name should be `path.string()`.
		// if the item with this name/path does not exist, NULL is returned.
		Item* get(const std::string& name) const;
		Item* get(const char* name) const;

		// get the file with the given path; fs::weakly_canonical() is called on the path.
		Item* get(const fs::path& path) const;

		// If an item with the name already exists, return it; else, create a new item, and return that. Note
		// that the `kind` argument is only used for creating new items.
		Item* get_or_add(int kind, std::string name);
		Item* get_or_add(int kind, const char* name);

		// same same, fs::weakly_canonical, etc.
		Item* get_or_add(int kind, const fs::path& path);

		// same, but infer the kind from the filename
		Item* get_or_add(const fs::path& path);

		/*
			Sort the items in the graph topologically (in dependency order). The returned array (if valid) contains
			a list (A) of lists (B), where the items in list (A) are sorted such that an in-order traversal (ie.
			starting at index 0) will ensure that, for any item, its dependencies are visited before it.

			The `roots` parameter specifies where to 'start' the sort; if it is empty, then an empty list is returned
			(it is not an error). Otherwise, the sorting algorithm treats the items in `roots` as the root of the search.

			In short, to generate the dependencies of (eg.) 'a.out', pass roots = { 'a.out' }.

			Each item in the list (B) can be compiled/built/whatever-ed in parallel -- items in the same sublist (B)
			do not have any inter-dependencies. This facilitates parallelisation of the build process, since you
			can build those items simultaneously.

			The return value is either Ok(the list of lists), or Err(a list of circular dependencies). In the error
			case, a chain of circularly-depending items are generated; for example, if [A, B, C] is returned, then
			there is a circle [A -> B -> C -> A].
		*/
		Result<std::vector<std::vector<Item*>>, std::vector<Item*>> topological_sort(std::vector<Item*> roots) const;

	private:
		std::unordered_map<std::string, Item*> items;

		// this is a helper method that tries as hard as possible to find the list of circular items
		std::vector<Item*> get_circular_depends(Item* root) const;
	};

	/*
		Return one of the KIND_* kinds, based on the filename. The rules are:
		1. If there is no extension, or the extension is '.exe', return KIND_EXE
		2. If the extension is '.o' or '.obj', return KIND_OBJECT
		3. If the extension is '.c', return KIND_C_SOURCE
		4. If the extension is one of '.cpp', '.cc', '.cxx', 'c++', return KIND_CPP_SOURCE
		5. If the extension is '.pch' or '.gch', return KIND_PCH
		6. If the extension is '.m', return KIND_OBJC_SOURCE
		7. If the extension is '.mm', return KIND_OBJCPP_SOURCE
		8. otherwise, return KIND_NONE
	*/
	int infer_kind_from_filename(const fs::path& filename);

	bool read_dependencies_from_file(Graph& graph, const fs::path& dependency_file);
}

// implementation
#if !NABS_DECLARATION_ONLY
namespace nabs::dep
{
	int infer_kind_from_filename(const fs::path& filename)
	{
		auto ext = filename.extension().string();
		if(ext == "" || ext == ".exe")
			return KIND_EXE;
		else if(ext == ".o" || ext == ".obj")
			return KIND_OBJECT;
		else if(ext == ".c")
			return KIND_C_SOURCE;
		else if(ext == ".cpp" || ext == ".cc" || ext == ".cxx" || ext == ".c++")
			return KIND_CPP_SOURCE;
		else if(ext == ".pch" || ext == ".gch")
			return KIND_PCH;
		else if(ext == ".m")
			return KIND_OBJC_SOURCE;
		else if(ext == ".mm")
			return KIND_OBJCPP_SOURCE;
		else
			return KIND_NONE;
	}

	void Item::depend(Item* item)
	{
		if(std::find(this->deps.begin(), this->deps.end(), item) != this->deps.end())
			return;

		item->dependents += 1;
		this->deps.push_back(item);
	}

	void Item::add_produced_by(Item* item)
	{
		this->depend(item);
		if(std::find(this->produced_by.begin(), this->produced_by.end(), item) == this->produced_by.end())
			this->produced_by.push_back(item);
	}

	Graph::~Graph()
	{
		for(auto& [ _, i ] : this->items)
			delete i;
	}

	std::vector<Item*> Graph::get_circular_depends(Item* root) const
	{
		// note that this function can be slow, because it's only called on the error path.
		if(root == nullptr)
		{
			// if the root is empty, then we couldn't find any root nodes;
			// so what we do is to sort the graph by # of dependents, get
			// a least-depended-on node (since there might be multiple),
			// and just traverse that.
			Item* shallowest = nullptr;

			for(auto& [ _, item ] : this->items)
			{
				if(shallowest == nullptr || item->dependents < shallowest->dependents)
					shallowest = item;
			}

			// we know this must be true, because the graph must have at least one
			// item, because if it was empty we would not have a circular chain.
			assert(shallowest != nullptr);
			return this->get_circular_depends(shallowest);
		}

		// perform a DFS until we find a chain that leads us back to the root.
		struct {
			std::vector<Item*> operator() (Item* root, Item* item, std::vector<Item*> chain)
			{
				chain.push_back(item);

				if(item == root)
					return chain;

				for(auto dep : item->deps)
				{
					auto foo = (*this)(root, dep, chain);
					if(!foo.empty())
						return foo;
				}

				return { };
			}

			std::vector<Item*> operator() (Item* root)
			{
				for(auto d : root->deps)
				{
					if(auto chain = (*this)(root, d, { }); !chain.empty())
						return chain;
				}
				return { };
			}
		} dfs;

		return dfs(root);
	}

	// based on https://stackoverflow.com/questions/4073119/topological-sort-with-grouping
	Result<std::vector<std::vector<Item*>>, std::vector<Item*>> Graph::topological_sort(std::vector<Item*> queue) const
	{
		if(this->items.empty() || queue.empty())
			return Ok(std::vector<std::vector<Item*>> { });

		std::set<Item*> visited;

		for(auto& [ name, item ] : this->items)
			item->level = 0;

		size_t max_level = 0;
		while(!queue.empty())
		{
			auto item = queue.back();
			queue.pop_back();

			visited.insert(item);
			for(auto dep : item->deps)
			{
				if(dep->level < item->level && visited.find(dep) != visited.end())
					return Err(this->get_circular_depends(item));

				if(item->level + 1 > dep->level)
				{
					dep->level = item->level + 1;
					max_level = std::max(dep->level, max_level);

					queue.push_back(dep);
					visited.erase(dep);
				}
			}
		}

		// finally, build the list based on the levels.
		auto sorted = std::vector<std::vector<Item*>>(1 + max_level);
		for(auto& [ _, item ] : this->items)
		{
			// we use `max_level - item->level` because obviously it must be
			// in reverse order (the level 0 ones must be built last)
			if(visited.find(item) != visited.end())
				sorted[max_level - item->level].push_back(item);
		}

		return Ok(sorted);
	}

	Item* Graph::add(int kind, const fs::path& path)
	{
		auto name = fs::weakly_canonical(path).string();
		if(auto it = this->items.find(name); it != this->items.end())
		{
			nabs::impl::int_warn("item '{}' already exists in the dependency graph", name);
			return it->second;
		}

		return (this->items[name] = new Item(kind, name));
	}

	Item* Graph::add(int kind, std::string name)
	{
		if(auto it = this->items.find(name); it != this->items.end())
		{
			nabs::impl::int_warn("item '{}' already exists in the dependency graph", name);
			return it->second;
		}

		return (this->items[name] = new Item(kind, name));
	}

	Item* Graph::get_or_add(int kind, const fs::path& path)
	{
		auto canon = fs::weakly_canonical(path).string();
		if(auto item = this->get(canon); item != nullptr)
			return item;

		// return this->add(kind, std::move(canon));
		return (this->items[canon] = new Item(kind, canon));
	}

	Item* Graph::get_or_add(int kind, std::string name)
	{
		if(auto item = this->get(name); item != nullptr)
			return item;

		return (this->items[name] = new Item(kind, name));
	}

	Item* Graph::get(const std::string& name) const
	{
		if(auto it = this->items.find(name); it != this->items.end())
			return it->second;
		else
			return nullptr;
	}

	Item* Graph::get(const fs::path& path) const
	{
		if(auto it = this->items.find(fs::weakly_canonical(path).string()); it != this->items.end())
			return it->second;
		else
			return nullptr;
	}

	Item* Graph::add_system_library(std::string name, std::optional<LibraryFinderOptions> opts)
	{
		auto item = this->add(KIND_SYSTEM_LIBRARY, std::move(name));
		if(opts.has_value())
			item->lib_finder_options = std::move(opts.value());
		else
			item->lib_finder_options = impl::global_state().rlock()->default_finder_opts;

		return item;
	}

	Item* Graph::add(const fs::path& path) { return this->add(infer_kind_from_filename(path), path); }
	Item* Graph::get_or_add(const fs::path& path) { return this->get_or_add(infer_kind_from_filename(path), path); }

	Item* Graph::get(const char* name) const { return this->get(std::string(name)); }
	Item* Graph::add(int kind, const char* name) { return this->add(kind, std::string(name)); }
	Item* Graph::get_or_add(int kind, const char* name) { return this->get_or_add(kind, std::string(name)); }

	bool read_dependencies_from_file(Graph& graph, const fs::path& dependency_file)
	{
		// it is *NOT* an error to not have this file!
		if(!fs::exists(dependency_file))
			return false;

		auto contents = fs::read_file(dependency_file).expect("failed to read file");
		auto lines = split_string_lines(contents);

		auto parse_till_space = [](std::string_view& line) -> std::string {
			std::string ret;
			line = trim_whitespace(line);

			// TODO: this is not very efficient, really.
			while(line.size() > 0)
			{
				char k = line.front();
				line.remove_prefix(1);

				if(k == ' ' || k == '\t')
					return ret;

				if(k == '\\')
				{
					char kk = line.front();
					line.remove_prefix(1);
					ret += kk;
				}
				else
				{
					ret += k;
				}
			}

			return ret;
		};

		// note: wrt. ':' in filenames, gcc correctly generates \: in the output, but clang *does not*.
		// if you do this (put ':' in your filenames), then this is entirely your fault, and i do not care.
		for(size_t i = 0; i < lines.size(); i++)
		{
			auto line = trim_whitespace(lines[i]);
			if(line.empty())
				continue;

			// what a shitshow. fuck you, bill gates
			auto target = parse_till_space(line);
			target = fs::path(target).make_preferred().string();

			if(target.empty() || target.back() != ':')
			{
				impl::int_warn("depfile: expected ':' following target");
				return true;
			}

			std::vector<std::string> deps;

			target.pop_back();
			while(true)
			{
				line = trim_whitespace(line);
				if(line.empty())
					break;

				if(line.front() != '\\')
				{
					auto dep = parse_till_space(line);
					if(!dep.empty())
					{
						dep = fs::path(dep).make_preferred().string();
						deps.push_back(std::move(dep));
					}
				}

				line = trim_whitespace(line);
				if(line.empty())
				{
					break;
				}
				else if(line.front() == '\\')
				{
					i += 1;
					line = trim_whitespace(lines[i]);
				}
			}

			int tk = infer_kind_from_filename(target);
			if(deps.empty())
			{
				// gcc/clang generate targets with no dependencies for headers, specifically so that if
				// they get deleted, everything doesn't grind to a halt. we emulate that in our dependency
				// system with FLAG_MIGHT_NOT_EXIST, so set that for the target iff it has no dependencies.
				graph.get_or_add(tk, target)->flags |= Item::FLAG_MIGHT_NOT_EXIST;
			}
			else
			{
				for(auto& dep : deps)
				{
					int dk = infer_kind_from_filename(dep);
					graph.get_or_add(tk, target)->depend(graph.get_or_add(dk, dep));
				}
			}
		}

		return true;
	}
}

namespace zpr
{
	template <>
	struct print_formatter<nabs::dep::Item*>
	{
		template <typename Cb> void print(nabs::dep::Item* item, Cb&& cb, format_args args)
		{
			detail::print_one(static_cast<Cb&&>(cb), static_cast<format_args&&>(args), item->name());
		}
	};
}
#endif // !NABS_DECLARATION_ONLY




// compiler specifics
namespace nabs
{
	Result<Compiler, std::string> find_c_compiler();
	Result<Compiler, std::string> find_cpp_compiler();

	CompilerFlags get_default_cflags();
	CompilerFlags get_default_cxxflags();
}

// implementation
#if !NABS_DECLARATION_ONLY
namespace nabs
{
	// methods for toolchain
	Toolchain& Toolchain::add_c_flags(const std::vector<std::string>& flags)
	{
		this->cflags.options.insert(this->cflags.options.end(), flags.begin(), flags.end());
		return *this;
	}

	Toolchain& Toolchain::add_cpp_flags(const std::vector<std::string>& flags)
	{
		this->cxxflags.options.insert(this->cxxflags.options.end(), flags.begin(), flags.end());
		return *this;
	}

	Toolchain& Toolchain::add_link_flags(const std::vector<std::string>& flags)
	{
		this->ldflags.options.insert(this->ldflags.options.end(), flags.begin(), flags.end());
		return *this;
	}

	Toolchain& Toolchain::add_c_includes(const std::vector<fs::path>& includes)
	{
		this->cflags.include_paths.insert(this->cflags.include_paths.end(), includes.begin(), includes.end());
		return *this;
	}

	Toolchain& Toolchain::add_cpp_includes(const std::vector<fs::path>& includes)
	{
		this->cxxflags.include_paths.insert(this->cxxflags.include_paths.end(), includes.begin(), includes.end());
		return *this;
	}

	Toolchain& Toolchain::use_c_precompiled_header(const fs::path& header)
	{
		this->cflags.precompiled_header = header;
		this->ldflags._precompiled_headers_for_linker.push_back(header);
		return *this;
	}

	Toolchain& Toolchain::use_cpp_precompiled_header(const fs::path& header)
	{
		this->cxxflags.precompiled_header = header;
		this->ldflags._precompiled_headers_for_linker.push_back(header);
		return *this;
	}

	Toolchain& Toolchain::define(const std::string& name, const std::string& value)
	{
		this->define_c(name, value);
		this->define_cpp(name, value);
		return *this;
	}

	Toolchain& Toolchain::define_c(const std::string& name, const std::string& value)
	{
		this->cflags.defines[name] = value;
		return *this;
	}

	Toolchain& Toolchain::define_cpp(const std::string& name, const std::string& value)
	{
		this->cxxflags.defines[name] = value;
		return *this;
	}

	Toolchain& Toolchain::use_threads(bool use)
	{
		this->cflags.need_pthreads = use;
		this->cxxflags.need_pthreads = use;
		this->ldflags.need_pthreads = use;
		return *this;
	}

	Toolchain& Toolchain::use_exceptions(bool use)
	{
		this->cflags.exceptions_disabled = !use;
		this->cxxflags.exceptions_disabled = !use;
		this->ldflags.exceptions_disabled = !use;
		return *this;
	}

	Toolchain& Toolchain::set_c_standard(const std::string& std)
	{
		this->cflags.language_standard = std;
		return *this;
	}

	Toolchain& Toolchain::set_cpp_standard(const std::string& std)
	{
		this->cxxflags.language_standard = std;
		return *this;
	}


	template <typename... Includes>
	Toolchain& Toolchain::add_c_includes(Includes&&... includes)
	{
		this->cflags.include_paths.insert(this->cflags.include_paths.end(), { static_cast<Includes&&>(includes)... });
		return *this;
	}

	template <typename... Includes>
	Toolchain& Toolchain::add_cpp_includes(Includes&&... includes)
	{
		this->cxxflags.include_paths.insert(this->cxxflags.include_paths.end(), { static_cast<Includes&&>(includes)... });
		return *this;
	}

	template <typename... Flags>
	Toolchain& Toolchain::add_c_flags(Flags&&... flags)
	{
		this->cflags.options.insert(this->cflags.options.end(), { static_cast<Flags&&>(flags)... });
		return *this;
	}

	template <typename... Flags>
	Toolchain& Toolchain::add_cpp_flags(Flags&&... flags)
	{
		this->cxxflags.options.insert(this->cxxflags.options.end(), { static_cast<Flags&&>(flags)... });
		return *this;
	}

	template <typename... Flags>
	Toolchain& Toolchain::add_link_flags(Flags&&... flags)
	{
		this->ldflags.options.insert(this->ldflags.options.end(), { static_cast<Flags&&>(flags)... });
		return *this;
	}


	namespace impl
	{
		static std::vector<fs::path> get_path_variable()
		{
			std::vector<fs::path> ret;
			auto var = os::get_environment_var("PATH");

			if(var.empty())
				return { };

			while(true)
			{
				auto i = var.find(':');
				if(i == std::string::npos)
				{
					ret.push_back(std::string(var));
					break;
				}
				else
				{
					auto comp = var.substr(0, i);
					ret.push_back(std::string(comp));
					var = var.substr(i + 1);
				}
			}

			return ret;
		}

		static fs::path find_file_in_path(const fs::path& file, const std::vector<fs::path>& path)
		{
			for(auto& p : path)
			{
				auto tmp = (p / file);
				if(fs::exists(tmp))
					return tmp;
			}

			return { };
		}

		static int get_compiler_kind(const fs::path& path)
		{
			// do a simple check first. note that this isn't *really* correct, because people might
			// do stupid things (eg. make `gcc` actually run `clang`... cough apple cough), so give
			// users a macro to force the strict check. Otherwise, this saves us from having to run
			// the compiler just to check its kind.
			#if !NABS_STRICT_COMPILER_CHECK
				if(auto name = path.filename(); name == "cl.exe" || name == "cl")
					return Compiler::KIND_MSVC_CL;
				else if(name == "clang" || name == "clang++")
					return Compiler::KIND_CLANG;
				else if(name == "gcc" || name == "g++")
					return Compiler::KIND_GCC;
			#endif

			// tbh the exit code doesn't matter, since we know it exists
			std::string out, err;
			cmd(path.string(), "--version").run(&out, &err);

			/*
				gcc:
				`gcc: fatal error: no input files`

				clang:
				`clang: error: no input files`

				msvc:
				`Microsoft (R) C/C++ Optimizing Compiler Version 19.23.28107 for x64`
			*/

			auto lines = split_string_lines(out);
			if(!lines.empty() && lines[0].find("Microsoft (R)") != std::string::npos)
			{
				return Compiler::KIND_MSVC_CL;
			}
			else
			{
				if(lines[0].find("clang") != std::string::npos)
					return Compiler::KIND_CLANG;

				else if(lines[0].find("gcc") != std::string::npos)
					return Compiler::KIND_GCC;
			}

			int_warn("could not determine the kind for compiler '{}'", path);
			return Compiler::KIND_UNKNOWN;
		}

		static Result<Compiler, std::string> get_compiler_from_env_var(const std::vector<fs::path>& path_env,
			const std::string& var, int lang)
		{
			if(fs::exists(var))
			{
				// first, check if this file exists "just like that" (in case an absolute path,
				// or something relative to the current dir, was provided)
				Compiler cc;
				cc.path = var;
				cc.kind = get_compiler_kind(var);
				cc.lang = lang;
				return Ok(std::move(cc));
			}
			else if(auto path = impl::find_file_in_path(var, path_env); !path.empty())
			{
				// if not, look for it in the path
				Compiler cc;
				cc.path = path;
				cc.kind = get_compiler_kind(path);
				cc.lang = lang;
				return Ok(std::move(cc));
			}
			else
			{
				return Err<std::string>(zpr::sprint("specified compiler '{}' does not exist", var));
			}
		}
	}

	Result<Compiler, std::string> find_c_compiler()
	{
		// TODO(#10): support cross-compilation better
		// basically we need a way to define the target, provide a sysroot, and
		// find a specific "brand" of compiler, if you will.

		// for now, we just use the platform #defines to see which compiler to
		// search for. if we see _WIN32 but nothing suggesting cygwin or mingw,
		// then use MSVC's cl.exe. else, just use `cc` like a unix system.

		// also, it could be the case that the compiler used to compile nabs.cpp
		// is *not* the same compiler you want to use for the rest of the project;
		// in these situations we also need to be able to have a method of selecting
		// the kind of compiler. for example, you might compile nabs with cl.exe, but
		// want to use mingw to compile the project.

		auto path_env = impl::get_path_variable();
		if(auto cc = os::get_environment_var("CC"); !cc.empty())
			return impl::get_compiler_from_env_var(path_env, cc, LANGUAGE_C);

	#if defined(_MSC_VER) || (defined(_WIN32) && !defined(__CYGWIN__) && !defined(__MINGW32__) && !defined(__MINGW64__))

		// assume we are always compiling for the host, because that is a reasonable assumption.
	#if defined(_M_X64) || defined(_M_AMD64)
		const char* ARCH = "x64";
	#elif defined(_M_IX86)
		const char* ARCH = "x86";
	#elif defined(_M_ARM64)
		const char* ARCH = "arm64";
	#elif defined(_M_ARM)
		const char* ARCH = "arm";
	#else
		#error "unknown host architecture"
	#endif

		// TODO(#11): not sure how msvc-finder deals with the situation where msvc is not installed
		Compiler ret { };
		ret.path = os::msvc_toolchain_binaries() / ARCH / "cl.exe";
		if(!fs::exists(ret.path))
			return Err<std::string>("'cl.exe' does not exist");

		ret.kind = Compiler::KIND_MSVC_CL;
		ret.lang = LANGUAGE_C;
		return Ok(ret);

	#else
		for(auto foo : { "clang", "gcc", "cc" })
		{
			if(auto exe = impl::find_file_in_path(foo, path_env); !exe.empty())
			{
				Compiler cc;
				cc.path = exe;
				cc.kind = impl::get_compiler_kind(exe);
				cc.lang = LANGUAGE_C;
				return Ok(std::move(cc));
			}
		}

		return Err<std::string>("no compiler in $PATH");
	#endif // _WIN32
	}

	Result<Compiler, std::string> find_cpp_compiler()
	{
		auto path_env = impl::get_path_variable();
		if(auto cc = os::get_environment_var("CXX"); !cc.empty())
			return impl::get_compiler_from_env_var(path_env, cc, LANGUAGE_CPP);

	#if defined(_MSC_VER) || (defined(_WIN32) && !defined(__CYGWIN__) && !defined(__MINGW32__) && !defined(__MINGW64__))

		// msvc uses cl.exe for both C and C++ -- the hard part is actually finding the damn thing.
		if(auto ret = find_c_compiler(); ret.ok())
		{
			ret->lang = LANGUAGE_CPP;
			return ret;
		}
		else
		{
			return Err<std::string>("could not find MSVC");
		}
	#else
		// basically, find 'c++' in the path.
		for(auto foo : { "clang++", "g++", "c++" })
		{
			if(auto exe = impl::find_file_in_path(foo, path_env); !exe.empty())
			{
				Compiler cxx;
				cxx.path = exe;
				cxx.kind = impl::get_compiler_kind(exe);
				cxx.lang = LANGUAGE_CPP;
				return Ok(std::move(cxx));
			}
		}

		return Err<std::string>("no compiler in $PATH");
	#endif // _WIN32
	}

	// TODO: again, support cross-compilation, prefix/sysroot, etc.
	Result<Toolchain, std::string> find_toolchain()
	{
		Toolchain ret;
		if(auto cc = find_c_compiler(); !cc.ok())
			return Err(cc.error());
		else
			ret.cc = cc.unwrap();

		if(auto cxx = find_cpp_compiler(); !cxx.ok())
			return Err(cxx.error());
		else
			ret.cxx = cxx.unwrap();

		if(ret.cxx.kind == Compiler::KIND_MSVC_CL)
		{
			// use link.exe, which is in the same folder as cl.exe
			ret.ld.path = ret.cxx.path.parent_path() / "link.exe";
			ret.ld.kind = Compiler::KIND_MSVC_LINK;
			ret.ld.lang = LANGUAGE_CPP;
		}
		else
		{
			// use the c++ compiler to link.
			ret.ld = ret.cxx;
		}

		ret.cflags = get_default_cflags();
		ret.cxxflags = get_default_cxxflags();

		// by default, create it.
		ret.ldflags.create_missing_folders = true;

		ret.cc.log_hook  = default_compiler_logger(false, "cc");
		ret.cxx.log_hook = default_compiler_logger(false, "cxx");
		ret.ld.log_hook  = default_compiler_logger(true, "ld");

	#if defined(__APPLE__)
		ret.objcc = ret.cc;
		ret.objcflags = get_default_cflags();

		ret.objcxx = ret.cxx;
		ret.objcxxflags = get_default_cxxflags();
	#endif

		return Ok(std::move(ret));
	}

	CompilerFlags get_default_cflags()
	{
		CompilerFlags ret;
		ret.language_standard = "c11";
		ret.create_missing_folders = true;
		ret.generate_header_dependencies = true;

		return ret;
	}

	CompilerFlags get_default_cxxflags()
	{
		CompilerFlags ret;
		ret.language_standard = "c++17";
		ret.create_missing_folders = true;
		ret.generate_header_dependencies = true;

		return ret;
	}
}
#endif // !NABS_DECLARATION_ONLY





// actual compiling
namespace nabs
{
	// compiles the provided *source* files straight to an executable.
	int compile_files(const Compiler& compiler, const CompilerFlags& opts, const std::optional<fs::path>& output,
		const std::vector<fs::path>& files);

	// passes -c (or /c) to the compiler to get an object file.
	int compile_to_object_file(const Compiler& compiler, const CompilerFlags& opts, const std::optional<fs::path>& output,
		const fs::path& files);

	// this actually calls 'c++' or 'cc', and not 'ld'.
	int link_object_files(const Compiler& compiler, const CompilerFlags& opts, const std::optional<fs::path>& output,
		const std::vector<fs::path>& files);

	template <typename... Args, typename = std::enable_if_t<((std::is_convertible_v<Args, fs::path>) && ...)>>
	inline int compile_files(const Compiler& compiler, const CompilerFlags& opts, const std::optional<fs::path>& output,
		Args&&... inputs)
	{
		return compile_files(compiler, opts, output, std::vector<fs::path> { inputs... });
	}

	// just overloads that don't take the output path.
	int compile_to_object_file(const Compiler& compiler, const CompilerFlags& opts, const fs::path& file);
	int compile_files(const Compiler& compiler, const CompilerFlags& opts, const std::vector<fs::path>& files);
	int link_object_files(const Compiler& compiler, const CompilerFlags& opts, const std::vector<fs::path>& files);

	fs::path get_default_object_filename(const Compiler& compiler, const CompilerFlags& opts, fs::path file);
	fs::path get_dependency_filename(const Compiler& compiler, const CompilerFlags& opts, fs::path file);
	fs::path get_default_pch_filename(const Compiler& compiler, const CompilerFlags& opts, fs::path header);

	Result<fs::path, int> compile_header_to_pch(const Compiler& compiler, const CompilerFlags& opts,
		const fs::path& header);
}

// implementation
#if !NABS_DECLARATION_ONLY
namespace nabs
{
	// this needs a forward declaration.
	namespace os
	{
		fs::path msvc_windows_sdk();
	}


	namespace impl
	{
		static void setup_exception_args(std::vector<std::string>& args, const Compiler& cmp, const CompilerFlags& opts)
		{
			if(cmp.kind == Compiler::KIND_CLANG || cmp.kind == Compiler::KIND_GCC)
			{
				if(opts.exceptions_disabled)
					args.push_back("-fno-exceptions");
			}
			else if(cmp.kind == Compiler::KIND_MSVC_CL)
			{
				args.push_back("/EHsc");
				if(opts.exceptions_disabled)
					args.push_back("-D_HAS_EXCEPTIONS=0");
			}
			else if(cmp.kind == Compiler::KIND_MSVC_LINK)
			{
				// nothing is required
			}
			else
			{
				impl::int_error("setup_exception_args(): unsupported compiler");
			}
		}

		static std::vector<std::string> setup_basic_args(const Compiler& cmp, const CompilerFlags& opts)
		{
			auto args = opts.options;

			if(cmp.kind == Compiler::KIND_CLANG || cmp.kind == Compiler::KIND_GCC)
			{
				for(auto& inc : opts.include_paths)
					args.push_back(zpr::sprint("-I{}", inc.string()));

				for(auto& fi : opts.forced_includes)
					args.push_back("-include"), args.push_back(fi.string());

				for(auto& [ def, val ] : opts.defines)
				{
					if(val.empty()) args.push_back(zpr::sprint("-D{}", def));
					else            args.push_back(zpr::sprint("-D{}={}", def, val));
				}

				if(!opts.language_standard.empty())
					args.push_back(zpr::sprint("-std={}", opts.language_standard));

				if(opts.need_pthreads)
					args.push_back("-pthread");
			}
			else if(cmp.kind == Compiler::KIND_MSVC_CL)
			{
				// this is very important.
				args.push_back("/nologo");

				for(auto& inc : opts.include_paths)
					args.push_back(zpr::sprint("/I{}", quote_argument(inc.string())));

				for(auto& fi : opts.forced_includes)
					args.push_back(zpr::sprint("/FI{}", quote_argument(fi.string())));

				if(!opts.language_standard.empty())
					args.push_back(zpr::sprint("/std:{}", opts.language_standard));

				for(auto& [ def, val ] : opts.defines)
				{
					if(val.empty()) args.push_back(zpr::sprint("/D{}", def));
					else            args.push_back(zpr::sprint("/D{}={}", def, val));
				}
			}
			else if(cmp.kind == Compiler::KIND_MSVC_LINK)
			{
				// this is very important.
				args.push_back("/NOLOGO");
			}
			else
			{
				impl::int_error("setup_basic_args(): unsupported compiler");
			}

			return args;
		}

		static std::optional<fs::path> get_intermediate_dir(const CompilerFlags& opts)
		{
			if(auto dir = opts.intermediate_output_folder; !dir.empty())
			{
				if(fs::exists(dir) && !fs::is_directory(dir))
				{
					impl::int_warn("intermediate file directory '{}' is not a folder, ignoring", dir.string());
					return { };
				}

				if(!fs::exists(dir))
					fs::create_directories(dir);

				return dir;
			}
			else
			{
				return { };
			}
		}

		static void create_missing_dirs(const CompilerFlags& opts, const fs::path& for_file)
		{
			if(auto p = for_file.parent_path(); p != for_file && opts.create_missing_folders && !fs::exists(p))
				fs::create_directories(p);
		}

		static fs::path ensure_exe_extension_if_necessary(const Compiler& cmp, const CompilerFlags& opts, fs::path f)
		{
			bool is_msvc = (cmp.kind == Compiler::KIND_MSVC_CL || cmp.kind == Compiler::KIND_MSVC_LINK);
			if(is_msvc && opts.msvc_ensure_exe_extension)
				return f.replace_extension(".exe");
			else
				return f;
		}

		static fs::path set_output_name(std::vector<std::string>& args, const Compiler& cmp, const CompilerFlags& opts,
			bool obj, const std::optional<fs::path>& output_name, const std::vector<fs::path>& files)
		{
			if(files.empty())
				return { };

			fs::path f;
			if(output_name.has_value())
			{
				f = *output_name;

				if(!obj)
					f = ensure_exe_extension_if_necessary(cmp, opts, f);
			}
			else
			{
				f = files[0];
				if(cmp.kind == Compiler::KIND_MSVC_CL || cmp.kind == Compiler::KIND_MSVC_LINK)
					f = f.replace_extension(obj ? ".obj" : ".exe");
				else
					f = f.replace_extension(obj ? (f.extension().string() + ".o") : "");
			}

			/*
				this is the behaviour we are implementing:
				1. if you specify an `intermediate_output_folder` in the options, then *all* object files
					will be generated there. for now, filenames are not uniqued, so if you have source files
					in different directories with the same name, don't use this option.

				2. following that, we then take the (first, should be only) source file, change its extension
					to '.obj', and generate it *in that folder*. end of story.

				3. if it is not specified, then the obj file is generated *next to the source file*, again with
					an extension of '.obj'. this is the default behaviour of gcc/clang.
			*/
			if(auto dir = get_intermediate_dir(opts); dir.has_value())
			{
				if(cmp.kind == Compiler::KIND_MSVC_CL)
				{
					// note: trailing backslash is important!!
					if(files.size() > 1)
						args.push_back(zpr::sprint("/Fo{}", quote_argument(dir->string() + "\\")));
					else
						args.push_back(zpr::sprint("/Fo{}", quote_argument((*dir / f.filename()).string())));

					// if we're compiling to exe, we still need to set the exe filename with /Fe
					if(!obj)
						args.push_back(zpr::sprint("/Fe:{}", quote_argument(f.string())));
				}
				else if(cmp.kind == Compiler::KIND_MSVC_LINK)
				{
					// i think we should be ok to just go to normal here as well.
					goto normal;
				}
				else if(cmp.kind == Compiler::KIND_CLANG || cmp.kind == Compiler::KIND_GCC)
				{
					// there's no special behaviour for this, because gcc/clang don't throw stupid obj
					// files everywhere if they're making an executable. so, we can just -o to set the output name.
					goto normal;
				}
				else
				{
					impl::int_error("set_output_name(): unsupported compiler");
				}
			}
			else
			{
			normal:
				create_missing_dirs(opts, f);

				if(cmp.kind == Compiler::KIND_MSVC_CL)
				{
					if(obj) args.push_back(zpr::sprint("/Fo{}", quote_argument(f.string())));
					else    args.push_back(zpr::sprint("/Fe:{}", quote_argument(f.string())));
				}
				else if(cmp.kind == Compiler::KIND_MSVC_LINK)
				{
					if(obj) impl::int_error("set_output_name(): cannot use 'link.exe' to produce object files");

					args.push_back(zpr::sprint("/OUT:{}", quote_argument(f.string())));
				}
				else if(cmp.kind == Compiler::KIND_CLANG || cmp.kind == Compiler::KIND_GCC)
				{
					args.push_back("-o");
					args.push_back(f.string());
				}
				else
				{
					impl::int_error("set_output_name(): unsupported compiler");
				}
			}

			return f;
		}

		static fs::path add_flags_for_compiling_to_obj(std::vector<std::string>& args, const Compiler& cmp,
			const CompilerFlags& opts, const std::optional<fs::path>& output, const fs::path& file)
		{
			if(cmp.kind == Compiler::KIND_CLANG || cmp.kind == Compiler::KIND_GCC)
			{
				args.push_back("-c");
				return impl::set_output_name(args, cmp, opts, /* obj: */ true, output, { file });
			}
			else if(cmp.kind == Compiler::KIND_MSVC_CL)
			{
				args.push_back("/nologo");

				args.push_back("/c");
				return impl::set_output_name(args, cmp, opts, /* obj: */ true, output, { file });
			}
			else
			{
				impl::int_error("add_flags_for_compiling_to_obj(): unsupported compiler");
			}
		}

		static void add_flags_for_precompiled_header(std::vector<std::string>& args, const Compiler& cmp,
			const CompilerFlags& opts, bool linking)
		{
			// if you use a precompiled header, it is *your* responsibility to make sure
			// that it is updated if/when necessary. the sanest way to do this is to enable
			// the `generate_header_dependencies` flag in the compiler options, and use the
			// dependency graph functionality (either via auto_*, or manually on your own) to
			// ensure that the pch is recompiled if anything it includes changes.
			if(linking)
			{
				if(cmp.kind == Compiler::KIND_MSVC_CL || cmp.kind == Compiler::KIND_MSVC_LINK)
				{
					// msvc generates an obj for the pch; we generated it with the default name,
					// so all we need to do is just link it in.
					// args.push_back(get_default_object_filename(cmp, opts, hdr).string());
					for(auto& pch : opts._precompiled_headers_for_linker)
						args.push_back(get_default_object_filename(cmp, opts, pch).string());
				}
			}
			else if(auto hdr = opts.precompiled_header; !hdr.empty())
			{
				if(cmp.kind == Compiler::KIND_CLANG || cmp.kind == Compiler::KIND_GCC)
				{
					args.push_back("-include");
					args.push_back(hdr.string());
				}
				else if(cmp.kind == Compiler::KIND_MSVC_CL)
				{
					auto pch = get_default_pch_filename(cmp, opts, hdr);
					auto quoted_pch = quote_argument(fs::weakly_canonical(pch).string());
					auto quoted_hdr = quote_argument(fs::weakly_canonical(hdr).string());

					args.push_back(zpr::sprint("/FI{}", quoted_hdr));
					args.push_back(zpr::sprint("/Yu{}", quoted_hdr));
					args.push_back(zpr::sprint("/Fp{}", quoted_pch));
				}
				else
				{
					impl::int_error("add_flags_for_precompiled_header(): unsupported compiler");
				}
			}
		}

		static void add_flags_for_dependency_file(std::vector<std::string>& args, const Compiler& cmp,
			const CompilerFlags& opts, const fs::path& file)
		{
			if(opts.generate_header_dependencies)
			{
				if(cmp.kind == Compiler::KIND_CLANG || cmp.kind == Compiler::KIND_GCC)
				{
					args.push_back("-MP");
					args.push_back("-MMD");

					args.push_back("-MF");
					args.push_back(get_dependency_filename(cmp, opts, file).string());
				}
				else if(cmp.kind == Compiler::KIND_MSVC_CL)
				{
					args.push_back("/showIncludes");
				}
				else
				{
					impl::int_error("add_flags_for_dependency_file(): unsupported compiler");
				}
			}
		}

		static void parse_msvc_show_includes_and_write_to_file(std::string& std_out, const fs::path& out_path, const fs::path& src_path,
			const fs::path& dep_path, bool write_file)
		{
			/*
				key assumptions:
				1. your system language is english (or whatever visual studio's language is)
					if it's not, this literally will not work.

				2. the first line of output from cl.exe is always the name of the source file,
					which we can safely skip

				3. if the line is (a) not the first line, and (b) does not start with "Note: including file",
					we print it out to stderr verbatim.
			*/

			auto escape_spaces = [](const fs::path& path) -> std::string {
				// obviously the path separator cannot be '\'
				std::string sv = path.string();
				std::replace(sv.begin(), sv.end(), '\\', '/');

				std::string ret;

				for(char c : sv)
				{
					if(c == ' ')
						ret += '\\';

					ret += c;
				}

				return ret;
			};

			auto write_wrapper = [write_file](auto&&... args) {
				if(write_file)
					zpr::fprint(static_cast<decltype(args)&&>(args)...);
			};

			if(std_out.empty())
				return;


			constexpr const char MAGIC_WORDS[] = "Note: including file:";

			auto lines = split_string_lines(std_out);
			lines.erase(lines.begin());

			auto windows_sdk = os::msvc_windows_sdk();

			FILE* dep_file = nullptr;
			if(write_file)
			{
				auto _dep_file = fs::fopen_wrapper(dep_path, "wb");
				if(!_dep_file.ok())
				{
					impl::int_warn("couldn't open dependency file '{}' for writing: {}", dep_path, _dep_file.error());
					return;
				}

				dep_file = _dep_file.unwrap();
			}

			write_wrapper(dep_file, "{}: {} ", escape_spaces(out_path.string()), escape_spaces(src_path.string()));

			// we are forced to accumulate these here, for reasons.
			std::vector<std::string> headers;

			for(auto& line : lines)
			{
				if(line.find(MAGIC_WORDS) == 0)
				{
					auto file = fs::path(trim_whitespace(line.substr(sizeof(MAGIC_WORDS) - 1)));

					// there's no -MMD equivalent of -MD for /showInclude, so we must manually
					// exclude files that appear in the windows SDK (treat them as system includes)
					if(write_file && (windows_sdk.empty() || file.lexically_relative(windows_sdk).empty()))
						headers.push_back(escape_spaces(file.string()));
				}
				else
				{
					fprintf(stderr, "%.*s\n", static_cast<int>(line.size()), line.data());
				}
			}

			// print the list of headers (using alt syntax, so we don't get the '[' and the ',')
			write_wrapper(dep_file, "{#}\n\n", headers);

			// for each header, now we want to generate a target with no dependencies, just like gcc/clang.
			// this allows compilation to keep going even if the header gets deleted. Note that we handle this
			// case specifically in read_dependencies_from_file as well.
			for(auto& h : headers)
				write_wrapper(dep_file, "{}: \n\n", h);

			write_wrapper(dep_file, "\n");
			fclose(dep_file);
		}

		static void add_flags_for_linking_objects(std::vector<std::string>& args, const Compiler& cmp, const CompilerFlags& opts,
			const std::vector<fs::path>& inputs)
		{
			for(auto& pre_inputs : opts.pre_additional_input_files)
				args.push_back(pre_inputs.string());

			for(auto& f : inputs)
				args.push_back(f.string());

			for(auto& post_inputs : opts.post_additional_input_files)
				args.push_back(post_inputs.string());

			// libraries go after.
			if(cmp.kind == Compiler::KIND_CLANG || cmp.kind == Compiler::KIND_GCC)
			{
				for(auto& lib : opts.library_paths)
					args.push_back(zpr::sprint("-L{}", lib.string()));

				for(auto& lib : opts.libraries)
					args.push_back(zpr::sprint("-l{}", lib));

				// TODO: this should be a runtime check, based on the target!
			#if defined(__APPLE__)
				if(cmp.kind == Compiler::KIND_CLANG)
				{
					for(auto& f : opts.frameworks)
						args.push_back("-framework"), args.push_back(f);
				}
			#endif
			}
			else if(cmp.kind == Compiler::KIND_MSVC_CL || cmp.kind == Compiler::KIND_MSVC_LINK)
			{
				if(cmp.kind == Compiler::KIND_MSVC_CL)
					args.push_back("/link");

				for(auto& dir : opts.library_paths)
					args.push_back(zpr::sprint("/LIBPATH:{}", quote_argument(dir.string())));

				// we assume that whoever "found" the library used the right compiler, so
				// all the entries in 'libraries' will be actual filenames with .lib/.dll extensions.
				for(auto& lib : opts.libraries)
					args.push_back(lib);
			}
			else
			{
				impl::int_error("unsupported compiler");
			}
		}
	}

	fs::path get_default_object_filename(const Compiler& cmp, const CompilerFlags& opts, fs::path file)
	{
		// i guess we emit the same file for every compiler.
		if(auto inter = impl::get_intermediate_dir(opts); inter.has_value())
		{
			if(cmp.kind == Compiler::KIND_CLANG || cmp.kind == Compiler::KIND_GCC)
				return *inter / (file.concat(".o"));
			else if(cmp.kind == Compiler::KIND_MSVC_CL || cmp.kind == Compiler::KIND_MSVC_LINK)
				return *inter / (file.replace_extension(".obj"));
			else
				impl::int_error("get_default_object_filename(): unsupported compiler");
		}
		else
		{
			if(cmp.kind == Compiler::KIND_CLANG || cmp.kind == Compiler::KIND_GCC)
				return file.concat(".o");
			else if(cmp.kind == Compiler::KIND_MSVC_CL || cmp.kind == Compiler::KIND_MSVC_LINK)
				return file.replace_extension(".obj");
			else
				impl::int_error("get_default_object_filename(): unsupported compiler");
		}
	}

	fs::path get_dependency_filename(const Compiler& cmp, const CompilerFlags& opts, fs::path file)
	{
		(void) cmp;

		// i guess we emit the same file for every compiler.
		if(auto inter = impl::get_intermediate_dir(opts); inter.has_value())
		{
			return *inter / (file.concat(".d"));
		}
		else
		{
			return file.concat(".d");
		}
	}

	fs::path get_default_pch_filename(const Compiler& cmp, const CompilerFlags& opts, fs::path header)
	{
		fs::path filename;
		if(cmp.kind == Compiler::KIND_CLANG)
			filename = header.concat(".pch");
		else if(cmp.kind == Compiler::KIND_GCC)
			filename = header.concat(".gch");
		else if(cmp.kind == Compiler::KIND_MSVC_CL)
			filename = header.concat(".pch");
		else
			impl::int_error("get_default_pch_filename(): unsupported compiler");

		if(auto inter = impl::get_intermediate_dir(opts); inter.has_value())
			return *inter / filename;

		else
			return filename;
	}

	Result<fs::path, int> compile_header_to_pch(const Compiler& cmp, const CompilerFlags& opts,
		const fs::path& header)
	{
		auto args = impl::setup_basic_args(cmp, opts);
		impl::setup_exception_args(args, cmp, opts);
		impl::add_flags_for_dependency_file(args, cmp, opts, header);

		auto pch = get_default_pch_filename(cmp, opts, header);

		// the pch either goes next to the header file, or in the intermediate directory
		// (just like object files)
		if(cmp.kind == Compiler::KIND_CLANG || cmp.kind == Compiler::KIND_GCC)
		{
			if(cmp.lang == LANGUAGE_CPP)
				args.push_back("-x"), args.push_back("c++-header");
			else if(cmp.lang == LANGUAGE_C)
				args.push_back("-x"), args.push_back("c-header");
			else if(cmp.lang == LANGUAGE_OBJC)
				args.push_back("-x"), args.push_back("objective-c-header");
			else if(cmp.lang == LANGUAGE_OBJCPP)
				args.push_back("-x"), args.push_back("objective-c++-header");

			auto pch = get_default_pch_filename(cmp, opts, header);
			args.push_back("-o");
			args.push_back(pch.string());

			args.push_back(header.string());

			if(cmp.log_hook)
				cmp.log_hook(pch, { header }, args);

			if(int status = cmd(cmp.path.string(), std::move(args)).run(); status != 0)
				return Err(status);

			return Ok(pch);
		}
		else if(cmp.kind == Compiler::KIND_MSVC_CL)
		{
			args.push_back("/c");
			args.push_back("/Yc");

			args.push_back(zpr::sprint("/Fp{}", impl::quote_argument(pch.string())));

			if(cmp.lang == LANGUAGE_C)
				args.push_back("/Tc");
			else if(cmp.lang == LANGUAGE_CPP)
				args.push_back("/Tp");
			else
				impl::int_error("unsupported language '{}'", cmp.lang);

			args.push_back(header.string());

			if(cmp.log_hook)
				cmp.log_hook(pch, { header }, args);

			// this also generates an object file that *MUST* be linked in. dumb asdf, but
			// put it in a predictable location (ie. where the other intermediate files go)
			auto pch_obj = get_default_object_filename(cmp, opts, header);
			args.push_back(zpr::sprint("/Fo{}", impl::quote_argument(pch_obj.string())));

			// i hate msvc
			std::string out;
			std::string err;

			int status = cmd(cmp.path.string(), std::move(args)).run(&out, &err);
			auto depfile = get_dependency_filename(cmp, opts, header);

			// stderr is irrelevant, because the people at microsoft don't know what's up
			impl::parse_msvc_show_includes_and_write_to_file(out, pch, header, depfile,
				/* write_file: */ (status == 0));

			// just in case, print the stderr as well.
			fprintf(stderr, "%s", err.c_str());

			if(status == 0) return Ok(pch);
			else            return Err(status);
		}
		else
		{
			impl::int_error("compile_header_to_pch(): unsupported compiler");
		}
	}



	int compile_to_object_file(const Compiler& cmp, const CompilerFlags& opts, const std::optional<fs::path>& output,
		const fs::path& file)
	{
		auto args = impl::setup_basic_args(cmp, opts);
		auto out_name = impl::add_flags_for_compiling_to_obj(args, cmp, opts, output, { file });
		impl::add_flags_for_precompiled_header(args, cmp, opts, /* linking: */ false);
		impl::setup_exception_args(args, cmp, opts);
		impl::add_flags_for_dependency_file(args, cmp, opts, file);

		for(auto& pre_inputs : opts.pre_additional_input_files)
			args.push_back(pre_inputs.string());

		args.push_back(file.string());

		for(auto& post_inputs : opts.post_additional_input_files)
			args.push_back(post_inputs.string());

		if(cmp.log_hook)
			cmp.log_hook(out_name, { file }, args);

		if(cmp.kind == Compiler::KIND_CLANG || cmp.kind == Compiler::KIND_GCC)
		{
			// this is easy, since the compiler writes it for us.
			return cmd(cmp.path.string(), std::move(args)).run();
		}
		else if(cmp.kind == Compiler::KIND_MSVC_CL)
		{
			// capture stdout and stderr
			std::string out;
			std::string err;

			int status = cmd(cmp.path.string(), std::move(args)).run(&out, &err);
			auto depfile = get_dependency_filename(cmp, opts, file);

			// stderr is irrelevant, because the people at microsoft don't know what's up
			impl::parse_msvc_show_includes_and_write_to_file(out, out_name, file, depfile,
				/* write_file: */ (status == 0));

			// just in case, print the stderr as well.
			fprintf(stderr, "%s", err.c_str());
			return status;
		}
		else
		{
			impl::int_error("compile_to_object_file(): unsupported compiler");
		}
	}




	int compile_files(const Compiler& cmp, const CompilerFlags& opts, const std::optional<fs::path>& output,
		const std::vector<fs::path>& files)
	{
		auto args = impl::setup_basic_args(cmp, opts);
		impl::setup_exception_args(args, cmp, opts);
		impl::add_flags_for_precompiled_header(args, cmp, opts, /* linking: */ true);

		auto out = impl::set_output_name(args, cmp, opts, /* obj: */ false, output, files);

		impl::add_flags_for_linking_objects(args, cmp, opts, files);

		if(cmp.log_hook)
			cmp.log_hook(out, files, args);

		// msvc's printing of the filename is seriously obnoxious
		if(cmp.kind == Compiler::KIND_MSVC_CL)
		{
			std::string out;
			auto status = cmd(cmp.path.string(), std::move(args)).run(&out);
			out.erase(0, 1 + out.find('\n'));

			fprintf(stdout, "%s", out.c_str());
			return status;
		}
		else
		{
			return cmd(cmp.path.string(), std::move(args)).run();
		}
	}

	int link_object_files(const Compiler& linker, const CompilerFlags& opts, const std::optional<fs::path>& output,
		const std::vector<fs::path>& files)
	{
		auto args = impl::setup_basic_args(linker, opts);
		impl::setup_exception_args(args, linker, opts);
		impl::add_flags_for_precompiled_header(args, linker, opts, /* linking: */ true);

		auto out = impl::set_output_name(args, linker, opts, /* obj: */ false, output, files);

		impl::add_flags_for_linking_objects(args, linker, opts, files);


		if(linker.log_hook)
			linker.log_hook(out, files, args);

		// construct the command, and run it.
		return cmd(linker.path.string(), std::move(args)).run();
	}





	int compile_to_object_file(const Compiler& compiler, const CompilerFlags& opts, const fs::path& file)
	{
		return compile_to_object_file(compiler, opts, { }, file);
	}

	int compile_files(const Compiler& compiler, const CompilerFlags& opts, const std::vector<fs::path>& files)
	{
		return compile_files(compiler, opts, { }, files);
	}

	int link_object_files(const Compiler& compiler, const CompilerFlags& opts, const std::vector<fs::path>& files)
	{
		return link_object_files(compiler, opts, { }, files);
	}
}
#endif // !NABS_DECLARATION_ONLY



// filesystem stuff
namespace nabs::fs
{
	// note that std::filesystem has been 'used' here,
	// but it is suggested to still prefix fs:: to things that come from std and
	// not from us.

	namespace impl
	{
		template <typename Iter, typename Predicate>
		inline void find_files_helper(std::vector<fs::path>& list, const fs::path& dir, Predicate&& pred)
		{
			// i guess this is not an error...?
			if(!fs::is_directory(dir))
				return;

			auto iter = Iter(dir);
			for(auto& ent : iter)
			{
				if((ent.is_regular_file() || ent.is_symlink()) && pred(ent))
					list.push_back(ent.path());
			}
		}
	}

	/*
		Search for files in the given directory (non-recursively), returning a list of paths
		that match the given predicate. Note that the predicate should accept a `std::filesystem::directory_entry`,
		*NOT* a `std::filesystem::path`.
	*/
	template <typename Predicate>
	inline std::vector<fs::path> find_files(const fs::path& dir, Predicate&& pred)
	{
		std::vector<fs::path> ret;
		impl::find_files_helper<fs::directory_iterator>(ret, dir, pred);
		return ret;
	}

	/*
		Same as `find_files`, but recursively traverses directories.
	*/
	template <typename Predicate>
	inline std::vector<fs::path> find_files_recursively(const fs::path& dir, Predicate&& pred)
	{
		std::vector<fs::path> ret;
		impl::find_files_helper<fs::recursive_directory_iterator>(ret, dir, pred);
		return ret;
	}

	/*
		Same as `find_files`, but in multiple (distinct) directories at once.
	*/
	template <typename Predicate>
	inline std::vector<fs::path> find_files(const std::vector<fs::path>& dirs, Predicate&& pred)
	{
		std::vector<fs::path> ret;
		for(auto& dir : dirs)
			impl::find_files_helper<fs::directory_iterator>(ret, dir, pred);
		return ret;
	}

	/*
		Same as `find_files_recursively`, but in multiple (distinct) directories at once.
	*/
	template <typename Predicate>
	inline std::vector<fs::path> find_files_recursively(const std::vector<fs::path>& dirs, Predicate&& pred)
	{
		std::vector<fs::path> ret;
		for(auto& dir : dirs)
			impl::find_files_helper<fs::recursive_directory_iterator>(ret, dir, pred);
		return ret;
	}



	/*
		Same semantics as `find_files`, but it returns files matching the given extension.
	*/
	std::vector<fs::path> find_files_ext(const fs::path& dir, std::string_view ext);

	/*
		Same semantics as `find_files_recursively`, but it returns files matching the given extension.
	*/
	std::vector<fs::path> find_files_ext_recursively(const fs::path& dir, std::string_view ext);

	/*
		Same semantics as `find_files`, but for multiple directories at once.
	*/
	std::vector<fs::path> find_files_ext(const std::initializer_list<fs::path>& dir, std::string_view ext);

	/*
		Same semantics as `find_files_recursively`, but for multiple directories at once.
	*/
	std::vector<fs::path> find_files_ext_recursively(const std::initializer_list<fs::path>& dir, std::string_view ext);



	/*
		Just a nonsense struct that reduces the amount of code you need to type to look for source files in
		disjoint folders with different extensions (eg. c and c++)
	*/
	struct FileFinder
	{
		inline FileFinder& add(const fs::path& dir, const std::string& ext)
		{
			this->list.push_back({ dir, ext });
			return *this;
		}

		inline std::vector<fs::path> find() const
		{
			std::vector<fs::path> ret;
			for(auto& [ dir, ext ] : this->list)
			{
				auto foo = find_files_ext_recursively(dir, ext);
				ret.insert(ret.end(), std::make_move_iterator(foo.begin()), std::make_move_iterator(foo.end()));
			}
			return ret;
		}

	private:
		std::vector<std::pair<fs::path, std::string>> list;
	};
}

#if !NABS_DECLARATION_ONLY
namespace nabs::fs
{
	std::vector<fs::path> find_files_ext(const fs::path& dir, std::string_view ext)
	{
		return find_files(dir, [&ext](auto ent) -> bool {
			return ent.path().extension() == ext;
		});
	}

	std::vector<fs::path> find_files_ext_recursively(const fs::path& dir, std::string_view ext)
	{
		return find_files_recursively(dir, [&ext](auto ent) -> bool {
			return ent.path().extension() == ext;
		});
	}

	std::vector<fs::path> find_files_ext(const std::initializer_list<fs::path>& dirs, std::string_view ext)
	{
		return find_files(dirs, [&ext](auto ent) -> bool {
			return ent.path().extension() == ext;
		});
	}

	std::vector<fs::path> find_files_ext_recursively(const std::initializer_list<fs::path>& dirs, std::string_view ext)
	{
		return find_files_recursively(dirs, [&ext](auto ent) -> bool {
			return ent.path().extension() == ext;
		});
	}



	// thanks to xXSuperCuberXx for teaching me about good naming conventions
	Result<FILE*, std::string> fopen_wrapper(const fs::path& path, const char* mode)
	{
		#if defined(_WIN32)
			// bill gates has a problem
			FILE* f = nullptr;
			if(fopen_s(&f, path.string().c_str(), mode) != 0)
				return Err<std::string>(os::strerror_wrapper());
		#else
			FILE* f = fopen(path.string().c_str(), mode);
			if(f == nullptr)
				return Err<std::string>(os::strerror_wrapper());
		#endif
		return Ok(f);
	}

	Result<std::string, std::string> read_file(const fs::path& file)
	{
		auto _f = fopen_wrapper(file, "rb");
		if(!_f.ok())
			return Err(_f.error());

		auto f = _f.unwrap();

		std::string input;
		fseek(f, 0, SEEK_END);

		auto fsize = static_cast<size_t>(ftell(f));
		fseek(f, 0, SEEK_SET);  //same as rewind(f);

		input.resize(fsize);
		if(auto read = fread(input.data(), 1, fsize, f); read < fsize)
		{
			if(ferror(f)) return Err<std::string>(os::strerror_wrapper());
			else          nabs::impl::int_warn("unexpected end of file (expected {} bytes, got only {})", fsize, read);
		}

		fclose(f);
		return Ok(std::move(input));
	}
}
#endif // !NABS_DECLARATION_ONLY





/*
	Self-rebuilding
	---------------

	Functionality that allows the build program to check if its source was changed, and if so rebuild
	itself. The details are not very important here; just know that the 'normal' way to use this is like so:

	int main(int argc, char** argv)
	{
		nabs::self_update(argc, argv, __FILE__);

		// your code
	}
*/
namespace nabs
{
	/*
		An "automagic" function that checks if the build recipe changed, and if so rebuilds it automatically,
		before re-running the recipe. This overload is only relevant if your build recipe comprises more
		than one source file. If not, use the other overload. Parameters:

		`argc`, `argv`      -- please pass exactly the `argc` and `argv` that you receive from main().
		`filenames`         -- a list of files that make up the build recipe
		`auto_find_include` -- whether or not to add '-I' options to the compiler invocation to help find 'nabs.h'
	*/
	void self_update(int argc, char** argv, std::vector<std::string> filenames, bool auto_find_include = false);

	/*
		The same as the overload above, but taking only one file. It is assumed then that in the #include directive
		for "nabs.h", either the correct relative path is provided, or "nabs.h" is in the same folder as your
		build recipe.
	*/
	void self_update(int argc, char** argv, const std::string& file);
}

// implementation
#if !NABS_DECLARATION_ONLY
namespace nabs
{
	namespace impl
	{
	#if defined(_WIN32)
		static constexpr const char* TMP_FILE_NAME = "__please_delete_me.exe";

		static HANDLE duplicate_handle_as_inheritable(HANDLE handle)
		{
			auto proc = GetCurrentProcess();

			HANDLE dst = nullptr;
			if(!DuplicateHandle(proc, handle, proc, &dst, 0, true, DUPLICATE_SAME_ACCESS))
				impl::int_error("DuplicateHandle(): {}", os::GetLastErrorAsString());

			return dst;
		}

		static HANDLE create_process(fs::path proc, const char* first, const std::vector<std::string>& args)
		{
			STARTUPINFO info;
			memset(&info, 0, sizeof(info));

			info.cb = sizeof(STARTUPINFO);
			// info.hStdInput  = duplicate_handle_as_inheritable(GetStdHandle(STD_INPUT_HANDLE));
			// info.hStdOutput = duplicate_handle_as_inheritable(GetStdHandle(STD_OUTPUT_HANDLE));
			// info.hStdError  = duplicate_handle_as_inheritable(GetStdHandle(STD_ERROR_HANDLE));
			// info.dwFlags |= STARTF_USESTDHANDLES;

			PROCESS_INFORMATION procinfo;
			memset(&procinfo, 0, sizeof(procinfo));

			auto cmdline = make_argument_array(first, args);
			auto cmdline_ = const_cast<LPSTR>(cmdline.c_str());

			// provide the lpApplicationPath explicitly here.
			auto exec_path = proc.string();

			auto result = CreateProcessA(
				exec_path.c_str(),  // LPCSTR                   lpApplicationName
				cmdline_,           // LPSTR                    lpCommandLine
				nullptr,            // LPSECURITY_ATTRIBUTES    lpProcessAttributes
				nullptr,            // LPSECURITY_ATTRIBUTES    lpThreadAttributes
				true,               // BOOL                     bInheritHandles
				0,                  // DWORD                    dwCreationFlags
				nullptr,            // LPVOID                   lpEnvironment
				nullptr,            // LPCSTR                   lpCurrentDirectory
				&info,              // LPSTARTUPINFO            lpStartupInfo
				&procinfo           // LPPROCESS_INFORMATION    lpProcessInformation
			);

			if(!result)
				impl::int_error("CreateProcess('{}'): {}", cmdline, os::GetLastErrorAsString());

			CloseHandle(procinfo.hThread);
			return procinfo.hProcess;
		}

		static void replace_self_windows(char** argv, fs::path new_name)
		{
			std::vector<std::string> arguments;
			for(size_t i = 1; argv[i] != nullptr; i++)
				arguments.push_back(argv[i]);

			// before we do anything, rename ourselves to something else.
			auto this_path = fs::canonical(argv[0]);

			auto tmp_path = this_path;
			tmp_path.replace_filename(TMP_FILE_NAME);

			if(!MoveFileEx(this_path.string().c_str(), tmp_path.string().c_str(), MOVEFILE_REPLACE_EXISTING))
				impl::int_error("MoveFileEx(): {}", os::GetLastErrorAsString());

			// and rename the incoming one to our old name.
			if(!MoveFileEx(new_name.string().c_str(), this_path.string().c_str(), MOVEFILE_REPLACE_EXISTING))
				impl::int_error("MoveFileEx(): {}", os::GetLastErrorAsString());

			auto child = create_process(this_path, this_path.string().c_str(), arguments);

			// wait for the child to finish
			if(WaitForSingleObject(child, INFINITE) != WAIT_OBJECT_0)
				impl::int_error("WaitForSingleObject(): {}", os::GetLastErrorAsString());

			DWORD status = 0;
			if(!GetExitCodeProcess(child, &status))
				impl::int_error("GetExitCodeProcess(): {}", os::GetLastErrorAsString());

			ExitProcess(status);
		}
	#endif

		static void rebuild_self(char** argv, std::vector<std::string> filenames, bool auto_find_include)
		{
			log("build recipe changed, rebuilding...");

			auto _cpp = find_cpp_compiler();
			if(!_cpp.ok())
			{
				impl::int_warn("aborting self-rebuild, could not find C++ compiler: {}", _cpp.error());
				return;
			}

			auto& cpp = _cpp.unwrap();

			// convert the filenames to paths. the caller should have already checked that
			// all the provided files actually exist.
			std::vector<fs::path> files;
			for(auto& f : filenames)
				files.push_back(fs::canonical(f));

			CompilerFlags flags;

			// find the required include directory
			if(auto_find_include)
			{
				fs::path include_dir;

				auto this_path = fs::path(__FILE__);
				if(!fs::exists(this_path))
				{
					impl::int_warn("somehow, the current header ('{}') does not exist, aborting self-rebuild", __FILE__);
					return;
				}

				// i can't be bothered to do some magic where we get the "shallowest" file and then
				// compose the relative path from that. so, for every file, if the include path does
				// not already exist, just include it again.
				for(auto& file : files)
				{
					auto& tmp = flags.include_paths;

					auto inc = this_path.lexically_relative(file);
					if(!inc.empty() && std::find(tmp.begin(), tmp.end(), inc) == tmp.end())
						tmp.push_back(inc);
				}
			}

			if(cpp.kind == Compiler::KIND_CLANG || cpp.kind == Compiler::KIND_GCC)
			{
				flags.language_standard = "c++17";
				flags.options.push_back("-fno-exceptions");
				flags.options.push_back("-Wextra");
				flags.options.push_back("-Wall");
				flags.options.push_back("-O2");

				// TODO(#8): checking for stdc++fs and/or c++fs needs to be more robust
				// eg. we should check if we were linked with libstdc++ or libc++, first of all
				// then, we also need to check the compiler version (or... the lib version?) properly

				// TODO(#9): expose stdc++fs checking to the user
				// whatever we know about the STL we want the user to be able to know also,
				// so that they can compile files using std::filesystem seamlessly as well.

				// this is a little annoying, but for older gcc we need to explicitly link std::filesystem.
			#if defined(__GNUC__) && !defined(__clang__) && (__GNUC__ < 9)
				flags.libraries.push_back("stdc++fs");
			#endif

				// also, pthreads is needed.
				flags.options.push_back("-pthread");
			}
			else if(cpp.kind == Compiler::KIND_MSVC_CL)
			{
				flags.language_standard = "c++17";
				flags.options.push_back("/O2");
				flags.options.push_back("/W3");
			}
			else
			{
				impl::int_warn("unsupported C++ compiler kind, aborting self-rebuild");
				return;
			}

			auto this_path = fs::path(argv[0]);
			assert(fs::exists(this_path));

			auto new_name = fs::path("__" + this_path.filename().string());
			int status = compile_files(cpp, flags, new_name, std::move(files));
			if(status != 0)
				impl::int_error("self-rebuild failed");

		#if defined(_WIN32)
			// on windows of course, this is a massive fucking pain.
			impl::replace_self_windows(argv, std::move(new_name));

		#else
			// for posix, it's just a rename + exec... ez.
			fs::rename(new_name, this_path);

			// now just exec without forking.
			if(execvp(this_path.string().c_str(), argv) < 0)
				impl::int_error("execvp(): {}", strerror(errno));
		#endif
		}
	}

	void self_update(int argc, char** argv, std::vector<std::string> filenames, bool auto_find_include)
	{
		if(argc < 1)
		{
			impl::int_warn("argc < 1??");
			return;
		}

	#if defined(_WIN32)
		if(fs::exists(impl::TMP_FILE_NAME) && fs::is_regular_file(impl::TMP_FILE_NAME))
		{
			// note: we don't care if this fails, we just don't want it to throw an exception.
			// for windows, we cannot remove the file from under a running exe, even though we
			// can rename it. so, in the "second" execution (ie. after a self-rebuild), the
			// __please_delete_me.exe file is still running (our parent), so we can't delete it.
			// in such a case, just leave it, and try to delete it next time.
			std::error_code ec;
			fs::remove(impl::TMP_FILE_NAME, ec);
		}
	#endif

		// first, get the modification time of this file:
		auto this_path = fs::path(argv[0]);
		if(!fs::exists(this_path))
		{
			impl::int_warn("argv[0] (= '{}') does not exist, something fishy is going on",
				fs::weakly_canonical(this_path).string());
			return;
		}

		bool need_rebuild = false;

		auto this_time = fs::last_write_time(this_path);
		for(auto& file : filenames)
		{
			// note: there's no requirement for any of the recipe sources to exist;
			// maybe you want to distribute a binary or something. if any of
			// the files don't exist we obviously can't rebuild, so exit.
			auto p = fs::path(file);
			if(!fs::exists(p))
				return;

			if(fs::last_write_time(p) > this_time)
			{
				need_rebuild = true;
				break;
			}
		}

		// also check the header itself.
		if(auto p = fs::path(__FILE__); fs::exists(p) && fs::last_write_time(p) > this_time)
			need_rebuild = true;

		if(need_rebuild)
			impl::rebuild_self(argv, std::move(filenames), auto_find_include);
	}

	void self_update(int argc, char** argv, const std::string& file)
	{
		self_update(argc, argv, std::vector<std::string> { file });
	}
}
#endif // !NABS_DECLARATION_ONLY
















/*
	msvc finder
	taken almost verbatim from flax, which is licensed under the Apache License 2.0:
	https://github.com/flax-lang/flax/blob/master/source/platform/msvcfinder.cpp

	that was in-turn adapted from jon blow's "microsoft_craziness.h", which is licensed under MIT:
	https://gist.github.com/machinamentum/a2b587a68a49094257da0c39a6c4405f

	note that we still implement these functions even on not-windows, but they always return
	an empty path.
*/
namespace nabs
{
	fs::path msvc_windows_sdk();
	fs::path msvc_toolchain_binaries();
	fs::path msvc_toolchain_libraries();
}

#if defined(_WIN32)

// implementation
#if !NABS_DECLARATION_ONLY
#pragma comment(lib, "ole32.lib")
#pragma comment(lib, "oleaut32.lib")
#pragma comment(lib, "advapi32.lib")

#include <combaseapi.h>

namespace nabs::impl
{
	// the techniques used here are with reference to Jon Blow's "microsoft_craziness.h" file.
	// it was released under the MIT license. see: https://gist.github.com/machinamentum/a2b587a68a49094257da0c39a6c4405f

	struct DECLSPEC_UUID("B41463C3-8866-43B5-BC33-2B0676F7F42E") DECLSPEC_NOVTABLE ISetupInstance : public IUnknown
	{
		STDMETHOD(GetInstanceId)(_Out_ BSTR* pbstrInstanceId) = 0;
		STDMETHOD(GetInstallDate)(_Out_ LPFILETIME pInstallDate) = 0;
		STDMETHOD(GetInstallationName)(_Out_ BSTR* pbstrInstallationName) = 0;
		STDMETHOD(GetInstallationPath)(_Out_ BSTR* pbstrInstallationPath) = 0;
		STDMETHOD(GetInstallationVersion)(_Out_ BSTR* pbstrInstallationVersion) = 0;
		STDMETHOD(GetDisplayName)(_In_ LCID lcid, _Out_ BSTR* pbstrDisplayName) = 0;
		STDMETHOD(GetDescription)(_In_ LCID lcid, _Out_ BSTR* pbstrDescription) = 0;
		STDMETHOD(ResolvePath)(_In_opt_z_ LPCOLESTR pwszRelativePath, _Out_ BSTR* pbstrAbsolutePath) = 0;
	};

	struct DECLSPEC_UUID("6380BCFF-41D3-4B2E-8B2E-BF8A6810C848") DECLSPEC_NOVTABLE IEnumSetupInstances : public IUnknown
	{
		STDMETHOD(Next)(_In_ ULONG celt, _Out_writes_to_(celt, *pceltFetched) ISetupInstance** rgelt,
			_Out_opt_ _Deref_out_range_(0, celt) ULONG* pceltFetched) = 0;

		STDMETHOD(Skip)(_In_ ULONG celt) = 0;
		STDMETHOD(Reset)(void) = 0;
		STDMETHOD(Clone)(_Deref_out_opt_ IEnumSetupInstances** ppenum) = 0;
	};

	struct DECLSPEC_UUID("42843719-DB4C-46C2-8E7C-64F1816EFD5B") DECLSPEC_NOVTABLE ISetupConfiguration : public IUnknown
	{
		STDMETHOD(EnumInstances)(_Out_ IEnumSetupInstances** ppEnumInstances) = 0;
		STDMETHOD(GetInstanceForCurrentProcess)(_Out_ ISetupInstance** ppInstance) = 0;
		STDMETHOD(GetInstanceForPath)(_In_z_ LPCWSTR wzPath, _Out_ ISetupInstance** ppInstance) = 0;
	};

	struct DECLSPEC_UUID("42B21B78-6192-463E-87BF-D577838F1D5C") DECLSPEC_NOVTABLE ISetupHelper : public IUnknown
	{
		STDMETHOD(ParseVersion)(_In_ LPCOLESTR pwszVersion, _Out_ PULONGLONG pullVersion) = 0;
		STDMETHOD(ParseVersionRange)(_In_ LPCOLESTR pwszVersionRange, _Out_ PULONGLONG pullMinVersion,
			_Out_ PULONGLONG pullMaxVersion) = 0;
	};

	struct VersionData
	{
		int32_t bestVersion[4];
		std::wstring bestName;
	};

	struct FindResult
	{
		int windowsVersion;
		std::string windowsSDKRoot;
		std::string vsBinDirectory;
		std::string vsLibDirectory;
	};

	std::wstring convertStringToWChar(const std::string& s)
	{
		if(s.empty())
			return L"";

		if(s.size() > INT_MAX)
			impl::int_error("string length %d is too large", s.size());

		int required = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, s.c_str(), (int) s.size(), NULL, 0);
		if(required == 0) impl::int_error("failed to convert string");


		auto buf = (LPWSTR) malloc(sizeof(WCHAR) * (required + 1));
		if(!buf) impl::int_error("failed to allocate buffer");

		auto ret = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, s.c_str(), (int) s.size(), buf, required);
		assert(ret > 0);

		auto wstr = std::wstring(buf, ret);
		free(buf);

		return wstr;
	}

	std::string convertWCharToString(const std::wstring& s)
	{
		if(s.empty())
			return "";

		if(s.size() > INT_MAX)
			impl::int_error("string length %d is too large", s.size());

		int required = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, s.c_str(), -1, NULL, 0, NULL, NULL);
		if(required == 0) impl::int_error("failed to convert wstring");

		auto buf = (char*) malloc(sizeof(char) * (required + 1));
		if(!buf) impl::int_error("failed to allocate buffer");

		auto ret = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, s.c_str(), -1, buf, required, NULL, NULL);
		assert(ret > 0);

		auto str = std::string(buf, ret - 1);
		free(buf);

		return str;
	}


	bool checkFileExists(const std::wstring& name)
	{
		auto attrib = GetFileAttributesW(name.c_str());
		return attrib != INVALID_FILE_ATTRIBUTES;
	}

	template <typename Visitor>
	bool visitFiles(const std::wstring& dir, VersionData* vd, Visitor&& visitor)
	{
		auto wildcard = dir + L"\\*";

		WIN32_FIND_DATAW findData;
		auto handle = FindFirstFileW(wildcard.c_str(), &findData);
		if(handle == INVALID_HANDLE_VALUE) return false;


		while(true)
		{
			// make sure it's a directory, and don't read '.' or '..'
			if((findData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) && (findData.cFileName[0] != '.'))
			{
				auto full = dir + L"\\" + findData.cFileName;
				visitor(findData.cFileName, full, vd);
			}

			auto success = FindNextFileW(handle, &findData);
			if(!success) break;
		}

		FindClose(handle);
		return true;
	}

	std::wstring readRegistryString(HKEY key, const std::wstring& name)
	{
		// If the registry data changes between the first and second calls to RegQueryValueExW,
		// we may fail to get the entire key, even though it told us initially that our buffer length
		// would be big enough. The only solution is to keep looping until we don't fail.

		DWORD required = 0;
		auto rc = RegQueryValueExW(key, name.c_str(), NULL, NULL, NULL, &required);
		if(rc != 0) return L"";

		wchar_t* value = 0;
		DWORD length = 0;

		while(true)
		{
			length = required + 2;
			value = (wchar_t*) malloc(length + 2);
			if(!value)
				return L"";

			DWORD type;
			rc = RegQueryValueExW(key, name.c_str(), NULL, &type, (LPBYTE) value, &length);
			if(rc == ERROR_MORE_DATA)
			{
				free(value);
				required = length;
				continue;
			}

			// only get strings
			if((rc != 0) || (type != REG_SZ))
			{
				free(value);
				return L"";
			}

			break;
		}

		auto num_wchars = length / 2;
		value[num_wchars] = 0;

		auto ret = std::wstring(value);
		free(value);

		return ret;
	}

	void getBestWin10Version(const std::wstring& shortName, const std::wstring& fullName, VersionData* vd)
	{
		// find the win10 subdir with the highest version number.
		int i0 = 0;
		int i1 = 0;
		int i2 = 0;
		int i3 = 0;

		auto gots = swscanf_s(shortName.c_str(), L"%d.%d.%d.%d", &i0, &i1, &i2, &i3);
		if(gots < 4) return;

		auto b0 = vd->bestVersion[0];
		auto b1 = vd->bestVersion[1];
		auto b2 = vd->bestVersion[2];
		auto b3 = vd->bestVersion[3];

		// short-circuiting ftw.
		if((b0 > i0) || (b1 > i1) || (b2 > i2) || (b3 > i3))
			return;

		vd->bestName = fullName;
		vd->bestVersion[0] = i0;
		vd->bestVersion[1] = i1;
		vd->bestVersion[2] = i2;
		vd->bestVersion[3] = i3;
	}

	void getBestWin8Version(const std::wstring& shortName, const std::wstring& fullName, VersionData* vd)
	{
		// find the win8 subdir with the highest version number.
		int i0 = 0;
		int i1 = 0;

		auto gots = swscanf_s(shortName.c_str(), L"winv%d.%d", &i0, &i1);
		if(gots < 2)
			return;

		auto b0 = vd->bestVersion[0];
		auto b1 = vd->bestVersion[1];

		// short-circuiting ftw.
		if((b0 > i0) || (b1 > i1))
			return;

		vd->bestName = fullName;
		vd->bestVersion[0] = i0;
		vd->bestVersion[1] = i1;
	}


	void findWindowsKitRoot(FindResult* result)
	{
		HKEY key;
		auto rc = RegOpenKeyExA(HKEY_LOCAL_MACHINE, "SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots", 0,
			KEY_QUERY_VALUE | KEY_WOW64_32KEY | KEY_ENUMERATE_SUB_KEYS, &key);

		if(rc != S_OK)
			return;

		// find a windows 10 thing
		auto win10root = readRegistryString(key, L"KitsRoot10");
		auto win8root = readRegistryString(key, L"KitsRoot81");

		if(!win10root.empty())
		{
			auto win10lib = win10root + L"Lib";

			VersionData vd;
			memset(&vd, 0, sizeof(VersionData));

			visitFiles(win10lib, &vd, &getBestWin10Version);

			if(!vd.bestName.empty())
			{
				result->windowsVersion = 10;
				result->windowsSDKRoot = convertWCharToString(vd.bestName);

				goto out;
			}
		}

		if(!win8root.empty())
		{
			auto win10lib = win10root + L"Lib";

			VersionData vd;
			memset(&vd, 0, sizeof(VersionData));

			visitFiles(win10lib, &vd, &getBestWin8Version);

			if(!vd.bestName.empty())
			{
				result->windowsVersion = 8;
				result->windowsSDKRoot = convertWCharToString(vd.bestName);

				goto out;
			}
		}

	out:
		RegCloseKey(key);
		return;
	}

	bool findVSToolchain(FindResult* result)
	{
		// for vs >= 2017, we need to do some COM stupidity.

		CoInitializeEx(NULL, COINIT_MULTITHREADED);

		GUID my_uid             = { 0x42843719, 0xDB4C, 0x46C2, { 0x8E, 0x7C, 0x64, 0xF1, 0x81, 0x6E, 0xFD, 0x5B } };
		GUID clsid_setupConfig  = { 0x177F0C4A, 0x1CD3, 0x4DE7, { 0xA3, 0x2C, 0x71, 0xDB, 0xBB, 0x9F, 0xA3, 0x6D } };

		ISetupConfiguration* config = NULL;
		auto hr = CoCreateInstance(clsid_setupConfig, NULL, CLSCTX_INPROC_SERVER, my_uid, (void**) &config);
		if(hr != S_OK) return false;

		IEnumSetupInstances* instances = NULL;
		hr = config->EnumInstances(&instances);
		if(hr != S_OK)  { config->Release(); return false; }
		if(!instances)  { config->Release(); return false; }

		ISetupInstance* inst = 0;
		uint64_t newestVersionNum = 0;

		// we look for the newest version that's installed, as opposed to the first.
		while(true)
		{
			ISetupInstance* instance = NULL;
			auto hr = instances->Next(1, &instance, NULL);
			if(hr != S_OK) break;

			BSTR versionString;
			uint64_t versionNum = 0;

			hr = instance->GetInstallationVersion(&versionString);
			if(hr != S_OK) continue;

			hr = ((ISetupHelper*) config)->ParseVersion(versionString, &versionNum);
			SysFreeString(versionString);

			if(hr != S_OK)
				continue;

			if(newestVersionNum == 0 || versionNum > newestVersionNum)
			{
				inst = instance;
				newestVersionNum = versionNum;
			}
			else
			{
				instance->Release();
			}
		}


		if(!inst)
		{
			config->Release();
			instances->Release();
			return false;
		}

		std::string vsRoot;
		{
			BSTR tmp;
			auto hr = inst->ResolvePath(L"VC", &tmp);
			if(hr != S_OK)
			{
				inst->Release();
				config->Release();
				instances->Release();
				return false;
			}

			vsRoot = convertWCharToString(std::wstring(tmp));
			SysFreeString(tmp);

			inst->Release();
		}

		std::string toolchainVersion;
		{
			auto path = zpr::sprint("{}\\Auxiliary\\Build\\Microsoft.VCToolsVersion.default.txt", vsRoot);
			auto file = os::open_file(path.c_str(), { });
			if(file == INVALID_HANDLE_VALUE)
				impl::int_error("msvc_finder(): VCToolsVersion file missing");

			while(true)
			{
				char buf[512] { };
				auto n = os::read_file(file, buf, 512);
				if(n <= 0)
					break;

				toolchainVersion += std::string_view(buf, n);
			}

			os::close_file(file);
			toolchainVersion = trim_whitespace(toolchainVersion);
		}

		std::string toolchainPath = zpr::sprint("{}\\Tools\\MSVC\\{}", vsRoot, toolchainVersion);
		if(checkFileExists(convertStringToWChar(toolchainPath)))
		{
		#if defined(_M_X64) || defined(_M_AMD64)
			const char* ARCH = "x64";
		#elif defined(_M_IX86)
			const char* ARCH = "x86";
		#elif defined(_M_ARM64)
			const char* ARCH = "arm64";
		#elif defined(_M_ARM)
			const char* ARCH = "arm";
		#else
			#error "unknown host architecture"
		#endif

			//* this is *HOST* architecture, so we can just use our defines.
			result->vsBinDirectory = zpr::sprint("{}\\bin\\Host{}", toolchainPath, ARCH);
			result->vsLibDirectory = zpr::sprint("{}\\lib", toolchainPath);

			config->Release();
			instances->Release();
			return true;
		}
		else
		{
			config->Release();
			instances->Release();
			return false;
		}
	}

	FindResult* performMSVCSearch()
	{
		static bool cached = false;
		static FindResult cachedResult;

		if(!cached)
		{
			memset(&cachedResult, 0, sizeof(FindResult));

			findWindowsKitRoot(&cachedResult);
			auto found = findVSToolchain(&cachedResult);
			if(!found) impl::int_error("backend: failed to find installed Visual Studio location!");

			cached = true;
		}

		return &cachedResult;
	}
}
#endif // !NABS_DECLARATION_ONLY
#endif // _WIN32

#if !NABS_DECLARATION_ONLY
namespace nabs::os
{
	#if defined(_WIN32)
		// note: only the sdk directory is cached, for now. it's the only one that is kinda used
		// more than onace (ie. after
		fs::path msvc_windows_sdk()
		{
			if(auto cache = impl::global_state().rlock()->cached_windows_sdk_root; !cache.empty())
				return cache;

			auto ret = impl::performMSVCSearch()->windowsSDKRoot;
			auto canon = fs::weakly_canonical(ret);

			return impl::global_state().map_write([&canon](auto& gs) -> auto {
				gs.cached_windows_sdk_root = canon;
				return canon;
			});
		}

		fs::path msvc_toolchain_binaries()
		{
			auto ret = impl::performMSVCSearch()->vsBinDirectory;
			return fs::weakly_canonical(ret);
		}

		fs::path msvc_toolchain_libraries()
		{
			auto ret = impl::performMSVCSearch()->vsLibDirectory;
			return fs::weakly_canonical(ret);
		}
	#else
		fs::path msvc_windows_sdk() { return { }; }
		fs::path msvc_toolchain_binaries() { return { }; }
		fs::path msvc_toolchain_libraries() { return { }; }
	#endif
}
#endif // !NABS_DECLARATION_ONLY



// give the users a way to print std::filesystem::path
namespace zpr
{
	template <>
	struct print_formatter<std::filesystem::path>
	{
		template <typename Cb>
		void print(const std::filesystem::path& x, Cb&& cb, format_args args)
		{
			auto s = x.string();
			detail::print_string(static_cast<Cb&&>(cb), s.c_str(), s.size(), std::move(args));
		}
	};
}




// implementation of misc things
#if !NABS_DECLARATION_ONLY
namespace nabs
{
	namespace os
	{
	#if defined(_WIN32)
		LPSTR GetLastErrorAsString()
		{
			DWORD error = GetLastError();
			return GetErrorCodeAsString(error);
		}

		LPSTR GetErrorCodeAsString(DWORD error)
		{
			// https://stackoverflow.com/questions/1387064/how-to-get-the-error-message-from-the-error-code-returned-by-getlasterror

			LPSTR messageBuffer = nullptr;

			DWORD size = FormatMessage(
				FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
				nullptr, error, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
				reinterpret_cast<LPSTR>(&messageBuffer), 0, nullptr);

			return messageBuffer;
		}
	#endif // _WIN32
	}

	std::string trim_whitespace(std::string s)
	{
		auto ltrim = [](std::string& s) -> std::string& {
			s.erase(0, s.find_first_not_of(" \t\n\r\f\v"));
			return s;
		};

		auto rtrim = [](std::string& s) -> std::string& {
			s.erase(s.find_last_not_of(" \t\n\r\f\v") + 1);
			return s;
		};

		return ltrim(rtrim(s));
	}

	std::string_view trim_whitespace(std::string_view s)
	{
		auto ltrim = [](std::string_view s) -> std::string_view {
			auto i = s.find_first_not_of(" \t\n\r\f\v");
			if(i != std::string::npos)
				s.remove_prefix(i);
			return s;
		};

		auto rtrim = [](std::string_view s) -> std::string_view {
			auto i = s.find_last_not_of(" \t\n\r\f\v");
			if(i != std::string::npos)
				s.remove_suffix(s.size() - i - 1);
			return s;
		};

		return ltrim(rtrim(s));
	}

	std::vector<std::string_view> split_string_lines(std::string& str)
	{
		std::vector<std::string_view> ret;
		std::string_view view = str;

		while(!view.empty())
		{
			size_t ln = view.find('\n');

			if(ln != std::string_view::npos)
			{
				// the windows ending is \r\n, so if the line ends with \r,
				// yeet it.
				if(view.back() == '\r')
					ln -= 1;
				ret.emplace_back(view.data(), ln);
				view.remove_prefix(ln + 1);
			}
			else
			{
				break;
			}
		}

		// account for the case when there's no trailing newline, and we still have some stuff stuck in the view.
		if(!view.empty())
		{
			if(view.back() == '\r')
				view.remove_suffix(1);

			ret.emplace_back(view.data(), view.length());
		}

		return ret;
	}

	namespace impl
	{
		template <typename Predicate>
		inline std::vector<std::string_view> split_string_pred(std::string& str, Predicate&& pred)
		{
			std::string_view view = str;
			std::vector<std::string_view> ret;
			while(true)
			{
				// chop the leading
				while(!view.empty() && pred(view.front()))
					view.remove_prefix(1);

				if(view.empty())
					break;

				size_t count = 0;
				while(count < view.size() && !pred(view[count]))
					count += 1;

				ret.emplace_back(view.data(), count);
				view.remove_prefix(count);
			}

			if(!view.empty())
				ret.emplace_back(view.data(), view.size());

			return ret;
		}
	}

	std::vector<std::string_view> split_string(std::string& str, char delim)
	{
		return impl::split_string_pred(str, [delim](char c) -> bool {
			return c == delim;
		});
	}

	std::vector<std::string_view> split_string_whitespace(std::string& str)
	{
		return impl::split_string_pred(str, [](char c) -> bool {
			return c == ' ' || c == '\t' || c == '\r' || c == '\n'
				|| c == '\v' || c == '\f';
		});
	}

	std::optional<int64_t> stoi(const std::string& s, int base)
	{
		if(s.empty())
			return { };

		char* tmp = 0;
		int64_t ret = strtoll(s.c_str(), &tmp, base);
		if(tmp != s.data() + s.size())
			return { };

		return ret;
	}

	std::optional<int64_t> stoi(std::string_view s, int base)
	{
		return stoi(std::string(s), base);
	}

	std::string os::get_environment_var(const std::string& name)
	{
	#if defined(_WIN32)
		std::string var;
		var.resize(4096, ' ');
		size_t len = 0;
		getenv_s(&len, var.data(), 4096, name.c_str());
		var.resize(len);

		return var;
	#else
		char* buf = getenv(name.c_str());
		if(buf == nullptr)
			return "";
		else
			return std::string(buf);
	#endif
	}

	namespace impl
	{
		zmt::Synchronised<GlobalState> state { };
		zmt::Synchronised<GlobalState>& global_state()
		{
			return state;
		}
	}
}
#endif // !NABS_DECLARATION_ONLY



/*
	Library finding
	---------------

	These functions are responsible for finding both system libraries, and local (project-specific)
	libraries. Right now, 'pkg-config' is the only supported mechanism to automatically search for
	system libraries. Failing that, you can always provide a prefix to search for locally-installed
	libraries in a specific location.
*/
namespace nabs
{
	Result<Library, std::string> find_library(const std::string& lib, const std::optional<LibraryFinderOptions>& opts);

	void use_library(const Library& lib, CompilerFlags& cflags, CompilerFlags& ldflags, bool separate_linking);
}

// implementation
#if !NABS_DECLARATION_ONLY
namespace nabs
{
	namespace impl
	{
		static bool check_pkg_config()
		{
			auto& gs = impl::global_state();
			if(gs.rlock()->pkg_config_checked)
			{
				return gs.rlock()->pkg_config_exists;
			}
			else
			{
				std::string foo;
				auto st = cmd("pkg-config", "--version").run(&foo);

				return gs.map_write([&st](auto& gs) -> bool {
					gs.pkg_config_checked = true;
					gs.pkg_config_exists = (st == 0);
					return gs.pkg_config_exists;
				});
			}
		}

		template <typename Mapper, typename = std::enable_if_t<!std::is_same_v<
			decltype(std::declval<Mapper>()(std::declval<std::string_view>())),
			void>>
		>
		static auto invoke_pkg_config(const char* opt, const char* kind, const std::string& lib, Mapper&& mapper, bool is_static)
			-> Result<std::vector<decltype(mapper(std::declval<std::string_view>()))>, std::string>
		{
			std::string tmp;
			std::vector<std::string> args = { opt, lib };
			if(is_static)
				args.push_back("--static");

			if(cmd("pkg-config", args).run(&tmp) != 0)
				return Err<std::string>(zpr::sprint("failed to get {} ({}) for '{}'", kind, opt, lib));

			tmp = trim_whitespace(tmp);
			return Ok(map(split_string(tmp, ' '), static_cast<Mapper&&>(mapper)));
		}

		template <typename Mapper, typename = std::enable_if_t<std::is_same_v<
			decltype(std::declval<Mapper>()(std::declval<std::string_view>())),
			void>>
		>
		static auto invoke_pkg_config(const char* opt, const char* kind, const std::string& lib, Mapper&& mapper, bool is_static)
			-> Result<void, std::string>
		{
			std::string tmp;
			std::vector<std::string> args = { opt, lib };
			if(is_static)
				args.push_back("--static");

			if(cmd("pkg-config", args).run(&tmp) != 0)
				return Err<std::string>(zpr::sprint("failed to get {} ({}) for '{}'", kind, opt, lib));

			tmp = trim_whitespace(tmp);
			map(split_string(tmp, ' '), static_cast<Mapper&&>(mapper));
			return Ok();
		}


		static Result<Library, std::string> find_with_pkg_config(const LibraryFinderOptions& opts,
			const std::string& name)
		{
			if(!impl::check_pkg_config())
				return Err<std::string>("pkg-config could not be found");

			Library lib;
			lib.name = name;
			if(cmd("pkg-config", "--modversion", name).run(&lib.version) != 0)
				return Err<std::string>(zpr::sprint("library '{}' not found by pkg-config", name));

			invoke_pkg_config("--cflags", "compiler flags", name, [&lib](auto sv) -> void {
				if(sv.find("-I") == 0)  lib.include_paths.push_back(sv.substr(2));
				else                    lib.compiler_flags.push_back(std::string(sv));
			}, opts.require_static_library);

			invoke_pkg_config("--libs", "linker flags", name, [&lib](auto sv) -> void {
				if(sv.find("-l") == 0)      lib.libraries.push_back(std::string(sv.substr(2)));
				else if(sv.find("-L") == 0) lib.library_paths.push_back(sv.substr(2));
				else                        lib.compiler_flags.push_back(std::string(sv));
			}, opts.require_static_library);

			return Ok(std::move(lib));
		}


		static Result<Library, std::string> find_in_unixlike_layout(const Compiler& compiler, const std::string& lib,
			const fs::path& base, const LibraryFinderOptions& opts)
		{
			// TODO(platform): this should probably be a runtime check for host/target
			#if defined(__APPLE__)
				constexpr const char* name_prefix       = "lib";
				constexpr const char* dylib_extension   = ".dylib";
				constexpr const char* static_extension  = ".a";
			#elif defined(_WIN32)
				constexpr const char* name_prefix       = "";
				constexpr const char* dylib_extension   = ".dll";
				constexpr const char* static_extension  = ".lib";
			#else
				constexpr const char* name_prefix       = "lib";
				constexpr const char* dylib_extension   = ".so";
				constexpr const char* static_extension  = ".a";
			#endif

			// either way, the return value doesn't change.
			Library ret { };

			ret.name = lib;
			ret.library_paths.push_back(base / "lib");
			ret.include_paths.push_back(base / "include");

			auto static_lib_name = fs::path(name_prefix + lib).replace_extension(static_extension);
			auto dynamic_lib_name = fs::path(name_prefix + lib).replace_extension(dylib_extension);

			auto static_lib = base / "lib" / static_lib_name;
			auto dynamic_lib = base / "lib" / dynamic_lib_name;

			auto foozle = [&](const fs::path& name) -> auto {
				if(compiler.kind == Compiler::KIND_MSVC_CL || compiler.kind == Compiler::KIND_MSVC_LINK)
					ret.libraries.push_back(name.string());
				else
					ret.libraries.push_back(lib);

				return Ok(ret);
			};


			// i don't actually know if this makes a difference.
			if(opts.prefer_static_library)
			{
				if(fs::exists(static_lib))
					return foozle(static_lib_name);

				else if(!opts.require_static_library && fs::exists(dynamic_lib))
					return foozle(dynamic_lib_name);
			}
			else
			{
				if(!opts.require_static_library && fs::exists(dynamic_lib))
					return foozle(dynamic_lib_name);

				else if(fs::exists(static_lib))
					return foozle(static_lib_name);
			}

			return Err<std::string>(zpr::sprint("library '{}' was not found in {}", lib, base));
		}
	}

	Result<Library, std::string> find_library(const Compiler& compiler, const std::string& lib,
		const std::optional<LibraryFinderOptions>& _opts)
	{
		const auto& opts = _opts.has_value()
			? _opts.value()
			: impl::global_state().rlock()->default_finder_opts;

		// search in the additional directories first
		if(opts.custom_searcher)
		{
			auto l = opts.custom_searcher(compiler, lib, opts);
			if(l.ok()) return l;
		}

		for(auto& dir : opts.additional_search_paths)
		{
			auto l = impl::find_in_unixlike_layout(compiler, lib, dir, opts);
			if(l.ok()) return l;

			if(opts.use_alternate_layout)
			{
				auto base = dir / lib / opts.alternate_layout_extra_prefix;
				auto l = impl::find_in_unixlike_layout(compiler, lib, base, opts);
				if(l.ok()) return l;
			}
		}

		if(opts.use_pkg_config)
		{
			auto l = impl::find_with_pkg_config(opts, lib);
			if(l.ok()) return l;
		}

	#if !defined(_WIN32)
		if(opts.use_usr)
		{
			auto l = impl::find_in_unixlike_layout(compiler, lib, "/usr", opts);
			if(l.ok()) return l;
		}

		if(opts.use_usr_local)
		{
			auto l = impl::find_in_unixlike_layout(compiler, lib, "/usr/local", opts);
			if(l.ok()) return l;
		}
	#endif

		return Err<std::string>("bsdf");
	}


	void use_library(const Library& lib, CompilerFlags& cf, CompilerFlags& lf, bool separate_linking)
	{
		append_vector(cf.options, lib.compiler_flags);
		append_vector(cf.include_paths, lib.include_paths);
		append_vector(cf.post_additional_input_files, lib.additional_compiler_inputs);

		append_vector(lf.library_paths, lib.library_paths);
		append_vector(lf.options, lib.linker_flags);
		append_vector(lf.libraries, lib.libraries);
		append_vector(lf.post_additional_input_files, lib.additional_linker_inputs);

		// if we are not linking as a separate step, then the compiler must also be invoked with the libraries.
		if(!separate_linking)
		{
			append_vector(cf.library_paths, lib.library_paths);
			append_vector(cf.options, lib.linker_flags);
			append_vector(cf.libraries, lib.libraries);
			append_vector(cf.post_additional_input_files, lib.additional_linker_inputs);
		}
	}
}
#endif




/*
	"automatic" compilation
	-----------------------
	I would put these in a namespace called `auto`, but obviously we can't do that.

	These functions allow you to approximate the functionality of a makefile with very few lines of
	code, and incorporate most of the expected functionality, including:
	- generating and using include dependencies for source files
	- topological sorting of targets, to build dependencies before their dependents
	- automatic usage and rebuilding of precompiled headers
	- only building a target if its dependencies are newer than it

	Notably, as mentioned below, the `generate_header_dependencies` flag should be set for all the
	compilers in the toolchain. Since a target (object, exe) is only rebuilt if its dependencies change,
	it is important that a source file 'depends-on' the header files that it includes.

	Otherwise, modifying a header file will not cause a source file to be rebuilt, and so the final
	executable file will not be rebuilt either. If you do not wish to use dependency information here,
	then you probably want to always compile all sources for an executable all the time, and these
	auto_* functions are not appropriate for that use case.

	Each of the auto_* functions are themselves documented, but the general workflow of using them
	is like this:

	1. Create an empty nabs::dep::Graph
	2. auto_executable_from_sources() -- Add dependency information for building an executable
		from a set of source files. Again, note the assumptions above, and/or see the documentation
		for this function for more specific details.

	3. auto_add_precompiled_header() -- If you are using a precompiled header, then you probably
		want to call this function so that (a) the PCH is rebuilt if the header changes, and (b) the
		PCH is rebuilt if any of the headers included by the PCH are changed.

	4. auto_build_targets() -- Automatically build the given targets based on the dependency graph
		that you just set up.
*/
namespace nabs
{
	/*
		These hooks are functions that are called by `auto_build_target` when compiling a file.

		1. modify_flags(&compiler, &flags, target, produced_by)
			Via the provided references to Compiler and CompilerFlags, you can change the compiler
			or its flags for a specific file. The changes only apply to that particular file.
	*/
	struct AutoCompileHooks
	{
		std::function<void (Compiler&, CompilerFlags&, dep::Item*, std::vector<dep::Item*>)> modify_flags;
	};

	/*
		Infer the compiler to use (in terms of a pair of Compiler and its CompilerFlags) for a given
		file, using its extension. Internally, this function uses `infer_kind_from_filename()` to
		get the kind of a file, and depending on whether it is KIND_C_SOURCE or KIND_CPP_SOURCE, returns
		`toolchain.cc + cflags` or `toolchain.cxx + cxxflags` respectively. If the inferred kind was not
		one of those two, then it returns an empty optional.

		There is rarely a need to call this manually, especially if you are already using the auto_* functions.
	*/
	std::optional<std::pair<Compiler, CompilerFlags>> auto_infer_compiler_from_file(const Toolchain& toolchain,
		const fs::path& path);

	/*
		Automatically populate the dependency graph with the right vertices and edges to build an executable
		from a set of source files, given just the output executable's path, and the list of source files.

		This also accepts a list of Items that are set as extra dependencies for *ALL* the sources and the
		final executable. You would mainly fill that list with Items representing KIND_SYSTEM_LIBRARY,
		although that is not enforced.

		By providing a library in that list, the following assumptions are made:
		1. Every source file in the list will be compiled with the list of compiler flags for all libraries
		2. The final executable will be linked with the linker flags (and/or `-l` `-L`) for all libraries

		This function is not responsible for actually building the target (use auto_build_target for that);
		it only sets up the dependency chains correctly.

		1. A dependency item is added for the output executable
		2. Each source file is 'depended-on' by its corresponding object file, named using get_default_object_filename
			(this function respects the `intermediate_output_folder` setting in the toolchain compilers)
		3. Read the "makefile-dependency-info" file (the .d file generated by gcc -MD) from disk at the
			default location (again, respecting `intermediate_output_folder`) to obtain header dependencies
		4. Add dependencies on the precompiled header, if it is set
		5. Add a 'produced-by' dependency from the executable file to each object file

		If the compilers in the toolchain are not setup to generate the dependency information
		(field `generate_header_dependencies`), it is not an error, but obviously you won't have that
		information. Importantly, this means that changing a header file *WILL NOT* force a recompile of the
		source files that include it.

		On success, it returns the item that represents the executable file. On failure, it returns an error
		message as a string -- typically this is the path of the target that failed to compile.
	*/
	Result<dep::Item*, std::string> auto_executable_from_sources(dep::Graph& graph, const Toolchain& toolchain,
		const fs::path& exe, const std::vector<fs::path>& files, const std::vector<dep::Item*>& extra_dependencies = { });


	/*
		This is just an overload of auto_executable_from_sources that takes a list of libraries as a list of names,
		assuming that all of them use the *default* LibraryFinderOptions. this is purely a convenience because
		it's easier to initialise.
	*/
	Result<dep::Item*, std::string> auto_executable_from_sources(dep::Graph& graph, const Toolchain& toolchain,
		const fs::path& exe_name, const std::vector<fs::path>& src_files, const std::vector<std::string>& libraries);


	/*
		Automatically build all the targets provided in the list, by using the dependency graph and
		sorting it in topological order. In case of success, an empty result is returned. In case of
		error, the path to the offending file is returned.

		There is nothing in this function that specifically requires the graph be setup using any of
		the auto_* functions, but it does make some assumptions:

		1. The `kind` field for the items in the graph are accurate. Namely, if a source file should
			be compiled with a C++ compiler, then it should have KIND_CPP_SOURCE.

		2. Executable files should have kind KIND_EXE, and will be linked together from object files
			using `toolchain.ld`

		3. Precompiled headers will be compiled using nabs::compile_header_to_pch(). Again, they
			should have a type KIND_PCH, and the header itself should be set to KIND_C_SOURCE or
			KIND_CPP_SOURCE correctly. We don't infer the type from the extension here, since some people
			(me) use `.h` for c++ headers too.

		4. Phony targets are always rebuilt.

		Overall, the principle of operation is that an item is rebuilt if its dependencies have a
		more recent modification time than the item itself (eg. a source file was modified, so it
		is "more recent" than its object file). This check is performed using std::filesystem::last_write_time.

		Lastly, you can provide a `AutoCompileHooks` structure which lets you:
		1. modify the flags just before compilation starts  -- modify_flags
	*/
	Result<void, std::string> auto_build_targets(const Toolchain& toolchain, const dep::Graph& graph,
		const std::vector<dep::Item*>& targets, const AutoCompileHooks& hooks = { });

	/*
		Identical to auto_build_targets, but it is just a wrapper that builds only one target.
	*/
	Result<void, std::string> auto_build_target(const Toolchain& toolchain, const dep::Graph& graph,
		dep::Item* target, const AutoCompileHooks& hooks = { });

	/*
		Identical to the overloads above, but using the provided thread pool for parallel job execution.
		This means that the overloads not taking a ThreadPool are single-threaded.
	*/
	Result<void, std::string> auto_build_targets(zmt::ThreadPool& pool, const Toolchain& toolchain,
		const dep::Graph& graph, const std::vector<dep::Item*>& targets, const AutoCompileHooks& hooks = { });

	Result<void, std::string> auto_build_target(zmt::ThreadPool& pool, const Toolchain& toolchain,
		const dep::Graph& graph, dep::Item* target, const AutoCompileHooks& hooks = { });



	/*
		Automatically add dependency information to build the given header as a precompiled header.
		This function uses `get_default_pch_filename()` as the name of the precompiled header; typically
		it will be the name of the header with an appended '.pch' or '.gch' extension. If
		`intermediate_output_folder` is set, then the header goes in there.

		Note that you must specify the language of the header (via the `lang` parameter), this is either
		LANGUAGE_C or LANGUAGE_CPP for now. We will not infer the type of the header from its extension since
		people (me) use '.h' for C++ headers as well.

		As long as `lang` is valid, this function always succeeds.
	*/
	void auto_add_precompiled_header(dep::Graph& graph, const Toolchain& toolchain,
		const fs::path& header, int lang);


	/*
		A very simple function that sets a filename's extension to '.exe' if the platform is windows.
	*/
	fs::path auto_set_extension_exe(fs::path path);
}

// implementation
#if !NABS_DECLARATION_ONLY
namespace nabs
{
	namespace impl
	{
		// TODO: something needs to be done about this to make it less stupid
		// basically, we want the `kind` of Item to be user-extendible, so function should also
		// be somehow user-extendible with their own kinds
		static bool is_kind_phonylike(int kind)
		{
			return kind == dep::KIND_PHONY || kind == dep::KIND_SYSTEM_LIBRARY;
		}

		// same thing here, make this user-extendable somehow
		static bool is_kind_sourcefile(int kind)
		{
			return kind == dep::KIND_C_SOURCE || kind == dep::KIND_CPP_SOURCE
				|| kind == dep::KIND_OBJC_SOURCE || kind == dep::KIND_OBJCPP_SOURCE;
		}


		/*
			this function takes a target that is known to be KIND_SYSTEM_LIBRARY, and walks its
			dependencies, resolving them with pkg-config if necessary, accumulating all the resolved
			libraries into the output parameter `depended_libs`.
		*/
		static Result<void, std::string> recursively_traverse_system_libraries(const Compiler& cmp, dep::Item* target,
			std::vector<const Library*>& depended_libs)
		{
			using namespace nabs::dep;

			assert(target->kind() == KIND_SYSTEM_LIBRARY);

			for(auto& dep : target->deps)
			{
				if(dep->kind() == KIND_SYSTEM_LIBRARY)
					recursively_traverse_system_libraries(cmp, dep, depended_libs);
			}

			auto lib = impl::global_state().map_read([&target](auto& gs) -> const Library* {
				if(auto it = gs.cached_libraries.find(target->name()); it != gs.cached_libraries.end())
					return &it->second;

				return nullptr;
			});

			if(lib == nullptr)
			{
				// TODO(#2): multiple jobs can call find_library at the same time
				// while semantics are correct (only one can update the cache at a time), it is wasted work
				auto library = find_library(cmp, target->name(), target->lib_finder_options);
				if(!library.ok())
					return Err<std::string>(zpr::sprint("could not find required library '{}'", target->name()));

				lib = impl::global_state().map_write([&target, &library](auto& gs) -> auto {
					gs.cached_libraries[target->name()] = std::move(library.unwrap());
					return &gs.cached_libraries[target->name()];
				});
			}

			assert(lib != nullptr);

			// TODO: not sure if it's worthwhile to check for duplicates here
			if(std::find(depended_libs.begin(), depended_libs.end(), lib) == depended_libs.end())
				depended_libs.push_back(lib);

			return Ok();
		}



		/*
			NOTE: don't call this directly, call the other overload.

			principle of operation:
			the topological sort already sorts things in dependency order. so, under normal circumstances
			this does not need to recursively traverse the dependencies of a target.

			however, because of performance reasons, we want to avoid calling pkg-config unless one or
			more object files are actually going to be compiled, so we have to put system library "uses"
			into the dependency graph.

			however, the "output" of pkg-config is just a bunch of flags, not a file on disk. so, it is
			hard (read: impossible) to "normally" carry that information from the dependency to the target
			by doing the topological traversal.

			So when we are compiling something that has KIND_SYSTEM_LIBRARY as a dependency, we do the
			recursively_traverse_system_libraries thing above.
		*/
		static Result<void, std::string> auto_compile_one_thing(dep::Item* target, const Toolchain& tc,
			const AutoCompileHooks& hooks)
		{
			using namespace nabs::dep;

			// system libraries should not be built directly, you should only "reach" them through the
			// recursively_traverse_system_libraries function.
			if(target->kind() == KIND_SYSTEM_LIBRARY)
				return Ok();


			bool target_exists = fs::exists(target->name());

			auto dependency_needs_rebuilding = [&target_exists](Item* target, Item* dep) -> Result<bool, std::string> {

				bool dep_exists = fs::exists(dep->name());
				bool dep_phony = is_kind_phonylike(dep->kind());

				if(!dep_phony && !dep_exists && !(dep->flags & Item::FLAG_MIGHT_NOT_EXIST))
				{
					return Err<std::string>(zpr::sprint("target '{}': required (non-phony) dependency '{}' does not exist",
						target->name(), dep->name()));
				}

				if(dep_phony || !dep_exists || !target_exists)
					return Ok(true);

				if(fs::last_write_time(dep->name()) > fs::last_write_time(target->name()))
					return Ok(true);

				return Ok(false);
			};


			auto use_libraries = [&](const Compiler& cmp, CompilerFlags& flags, bool linking) -> Result<void, std::string> {

				std::vector<const Library*> depended_libs;

				for(auto& dep : target->deps)
				{
					// we already handled the non-weak dependencies, so skip them.
					if(dep->kind() != KIND_SYSTEM_LIBRARY)
						continue;

					// for system libraries, they are also considered phony -- so they will always be
					// "rebuilt", which involves (a) pkg-config-ing them if not cached, and more importantly
					// (b) adding to the depended_libs list, which our object file will then use.
					if(auto foo = dependency_needs_rebuilding(target, dep); !foo.ok())
					{
						return Err(foo.error());
					}
					else if(foo.unwrap())
					{
						if(auto r = recursively_traverse_system_libraries(cmp, dep, depended_libs); !r.ok())
							return Err(r.error());
					}
				}

				for(auto lib : depended_libs)
				{
					CompilerFlags dummy;
					if(linking) use_library(*lib, dummy, flags, /* separate_linking: */ true);
					else        use_library(*lib, flags, dummy, /* separate_linking: */ true);
				}

				return Ok();
			};

			// this is a check that the target is phonyLIKE -- not that it is actually KIND_PHONY.
			// only "pure" phony (ie. really phony) targets are never built, but some targets that
			// are phonylike (eg. system libraries) still need some "build steps" (eg. running pkg-config)
			// this is similar to how make phony targets still have commands that must be run. So, we must
			// ensure that, even if the dependencies are up-to-date, a phonylike target is still 'built'.
			bool rebuild = is_kind_phonylike(target->kind());

			for(auto& dep : target->deps)
			{
				// NOTE: we don't check for KIND_SYSTEM_LIBRARY, since we know it's weak.
				if(dep->kind() == KIND_SYSTEM_LIBRARY)
					continue;

				if(auto foo = dependency_needs_rebuilding(target, dep); !foo.ok())
					return Err(foo.error());
				else
					rebuild |= foo.unwrap();

			}

			// now, we check if the target is actually KIND_PHONY. in this case, it has no commands that it
			// needs to run, so we quit now. note that phonyLIKE targets get past here and still get to "build".
			if(!rebuild || target->kind() == KIND_PHONY || is_kind_sourcefile(target->kind()))
				return Ok();

			if(target->kind() == KIND_OBJECT)
			{
				const auto& prod = target->produced_by;
				if(prod.size() != 1)
					impl::int_error("auto_build_targets(): produced_by for object files must have exactly 1 item");

				Compiler cmp;
				CompilerFlags flags;

				auto src = prod[0];
				if(src->kind() == KIND_C_SOURCE)
					cmp = tc.cc, flags = tc.cflags;
				else if(src->kind() == KIND_CPP_SOURCE)
					cmp = tc.cxx, flags = tc.cxxflags;
				else if(src->kind() == KIND_OBJC_SOURCE)
					cmp = tc.objcc, flags = tc.objcflags;
				else if(src->kind() == KIND_OBJCPP_SOURCE)
					cmp = tc.objcxx, flags = tc.objcxxflags;
				else
					impl::int_error("auto_build_targets(): unsupported source file '{}'", src->name());

				use_libraries(cmp, flags, /* linking: */ false);

				if(hooks.modify_flags)
					hooks.modify_flags(cmp, flags, target, target->produced_by);

				if(compile_to_object_file(cmp, flags, target->name(), src->name()) != 0)
					return Err<std::string>(target->name());
			}
			else if(target->kind() == KIND_PCH)
			{
				const auto& prod = target->produced_by;
				if(prod.size() != 1)
					impl::int_error("auto_build_targets(): produced_by for precompiled headers must have exactly 1 item");

				Compiler cmp;
				CompilerFlags flags;

				auto src = prod[0];
				if(src->kind() == KIND_C_SOURCE)
					cmp = tc.cc, flags = tc.cflags;
				else if(src->kind() == KIND_CPP_SOURCE)
					cmp = tc.cxx, flags = tc.cxxflags;
				else if(src->kind() == KIND_OBJC_SOURCE)
					cmp = tc.objcc, flags = tc.objcflags;
				else if(src->kind() == KIND_OBJCPP_SOURCE)
					cmp = tc.objcxx, flags = tc.objcxxflags;
				else
					impl::int_error("auto_build_targets(): unsupported source file '{}'", src->name());

				use_libraries(cmp, flags, /* linking: */ false);

				if(hooks.modify_flags)
					hooks.modify_flags(cmp, flags, target, target->produced_by);

				if(!compile_header_to_pch(cmp, flags, src->name()).ok())
					return Err<std::string>(target->name());
			}
			else if(target->kind() == KIND_EXE)
			{
				auto ld = tc.ld;
				auto ldflags = tc.ldflags;

				use_libraries(ld, ldflags, /* linking: */ true);

				if(hooks.modify_flags)
					hooks.modify_flags(ld, ldflags, target, target->produced_by);

				auto prod = map(target->produced_by, [](auto x) { return fs::path(x->name()); });
				if(link_object_files(ld, ldflags, target->name(), prod) != 0)
					return Err<std::string>(target->name());
			}
			else
			{
				impl::int_error("auto_build_targets(): unknown target kind '{}'", target->kind());
			}

			return Ok();
		}
	}


	Result<void, std::string> auto_build_targets(const Toolchain& toolchain, const dep::Graph& graph,
		const std::vector<dep::Item*>& targets, const AutoCompileHooks& hooks)
	{
		auto order = graph.topological_sort(targets)
			.expect("failed to find a valid dependency order");

		for(auto& targets : order)
		{
			for(auto& target : targets)
			{
				if(auto e = impl::auto_compile_one_thing(target, toolchain, hooks); !e.ok())
					return e;
			}
		}

		return Ok();
	}

	Result<void, std::string> auto_build_targets(zmt::ThreadPool& pool, const Toolchain& toolchain,
		const dep::Graph& graph, const std::vector<dep::Item*>& targets, const AutoCompileHooks& hooks)
	{
		auto order = graph.topological_sort(targets)
			.expect("failed to find a valid dependency order");

		using ResultTy = Result<void, std::string>;

		for(auto& targets : order)
		{
			std::vector<zmt::future<void>> futures;
			zmt::wait_queue<ResultTy> results;

			for(auto& target : targets)
			{
				futures.push_back(pool.run([&toolchain, &hooks, &target, &results]() {
					auto ret = impl::auto_compile_one_thing(target, toolchain, hooks);
					results.push(std::move(ret));
				}));
			}

			// pop the results
			ResultTy ret = Ok();
			size_t processed = 0;
			while(processed < futures.size())
			{
				auto result = results.pop();
				processed += 1;

				if(!result.ok())
				{
					ret = Err(result.error());
					break;
				}
			}

			// wait for the jobs to terminate
			zmt::futures::wait(futures);
			if(!ret.ok())
				return ret;
		}

		return Ok();
	}



	Result<void, std::string> auto_build_target(zmt::ThreadPool& pool, const Toolchain& toolchain,
		const dep::Graph& graph, dep::Item* target, const AutoCompileHooks& hooks)
	{
		return auto_build_targets(pool, toolchain, graph, { target }, hooks);
	}

	Result<void, std::string> auto_build_target(const Toolchain& toolchain, const dep::Graph& graph,
		dep::Item* target, const AutoCompileHooks& hooks)
	{
		return auto_build_targets(toolchain, graph, { target }, hooks);
	}




	// TODO(#3): missing API to build shared and static libraries
	// probably refactor this out into more constituent parts (eg. auto_objects_from_sources) and reuse those.
	Result<dep::Item*, std::string> auto_executable_from_sources(dep::Graph& graph, const Toolchain& toolchain,
		const fs::path& _exe_name, const std::vector<fs::path>& src_files, const std::vector<dep::Item*>& extra_dependencies)
	{
		using namespace dep;

		auto exe_name = impl::ensure_exe_extension_if_necessary(toolchain.ld, toolchain.ldflags, _exe_name);
		auto exe_file = graph.get_or_add(KIND_EXE, exe_name);

		for(auto edep : extra_dependencies)
			exe_file->depend(edep);

		for(auto& f : src_files)
		{
			auto _opt = auto_infer_compiler_from_file(toolchain, f);
			if(!_opt.has_value())
			{
				impl::int_warn("auto_executable_from_sources(): could not infer compiler for source file '{}'", f);
				return Err<std::string>(f.string());
			}

			auto [ compiler, flags ] = *_opt;

			auto src = graph.get_or_add(f);
			auto obj = graph.get_or_add(get_default_object_filename(compiler, flags, f));

			// setup the dependency from obj <- source
			obj->add_produced_by(src);
			exe_file->add_produced_by(obj);

			// depend on the libraries
			for(auto edep : extra_dependencies)
				obj->depend(edep);

			// also setup the header depends for the source
			read_dependencies_from_file(graph, get_dependency_filename(compiler, flags, f));

			// lastly, check precompiled headers.
			if(auto pch = flags.precompiled_header; !pch.empty())
			{
				auto pchname = get_default_pch_filename(compiler, flags, pch);
				obj->depend(graph.get_or_add(KIND_PCH, pchname));
			}
		}

		return Ok(exe_file);
	}

	Result<dep::Item*, std::string> auto_executable_from_sources(dep::Graph& graph, const Toolchain& toolchain,
		const fs::path& exe_name, const std::vector<fs::path>& src_files, const std::vector<std::string>& libraries)
	{
		std::vector<dep::Item*> libs;
		for(auto& lib : libraries)
			libs.push_back(graph.add_system_library(lib));

		return auto_executable_from_sources(graph, toolchain, exe_name, src_files, libs);
	}

	std::optional<std::pair<Compiler, CompilerFlags>> auto_infer_compiler_from_file(const Toolchain& toolchain,
		const fs::path& path)
	{
		using namespace dep;
		auto kind = infer_kind_from_filename(path);

		if(kind == KIND_C_SOURCE)
			return std::make_pair(toolchain.cc, toolchain.cflags);
		else if(kind == KIND_CPP_SOURCE)
			return std::make_pair(toolchain.cxx, toolchain.cxxflags);
		else if(kind == KIND_OBJC_SOURCE)
			return std::make_pair(toolchain.objcc, toolchain.objcflags);
		else if(kind == KIND_OBJCPP_SOURCE)
			return std::make_pair(toolchain.objcxx, toolchain.objcxxflags);
		else
			return { };
	}

	void auto_add_precompiled_header(dep::Graph& graph, const Toolchain& tc, const fs::path& header, int lang)
	{
		using namespace dep;

		int hdr_kind = KIND_NONE;

		const Compiler* cmp = nullptr;
		const CompilerFlags* opts = nullptr;

		if(lang == LANGUAGE_C)            hdr_kind = KIND_C_SOURCE, cmp = &tc.cc, opts = &tc.cflags;
		else if(lang == LANGUAGE_CPP)     hdr_kind = KIND_CPP_SOURCE, cmp = &tc.cxx, opts = &tc.cxxflags;
		else if(lang == LANGUAGE_OBJC)    hdr_kind = KIND_OBJC_SOURCE, cmp = &tc.objcc, opts = &tc.objcflags;
		else if(lang == LANGUAGE_OBJCPP)  hdr_kind = KIND_OBJCPP_SOURCE, cmp = &tc.objcxx, opts = &tc.objcxxflags;
		else                                    impl::int_error("unsupported language '{}'", lang);

		assert(cmp != nullptr && opts != nullptr);
		auto pch_name = get_default_pch_filename(*cmp, *opts, header);

		auto hdr = graph.get_or_add(hdr_kind, header);
		auto pch = graph.get_or_add(KIND_PCH, pch_name);

		pch->add_produced_by(hdr);

		// also read the dependencies
		read_dependencies_from_file(graph, get_dependency_filename(*cmp, *opts, header));
	}

	fs::path auto_set_extension_exe(fs::path path)
	{
		#if defined(_WIN32)
			path.replace_extension(".exe");
		#endif

		return path;
	}
}
#endif








/*
	Version History
	===============

	0.1.0 - 03/05/2021
	------------------
	Initial release.
*/
