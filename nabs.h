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

	There is none.




	Version History
	===============

	0.1.0 - 20/04/2021
	------------------
	Initial release.
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

	the code is included verbatim, but documentation has been removed.

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

	Version 2.1.12
	==============
*/

#include <cfloat>
#include <cstddef>
#include <cstdint>

#define ZPR_DO_EXPAND(VAL)  VAL ## 1
#define ZPR_EXPAND(VAL)     ZPR_DO_EXPAND(VAL)

// this weird macro soup is necessary to defend against the case where you just do
// #define ZPR_FOO_BAR, but without a value -- we want to treat that as a TRUE.
#if !defined(ZPR_HEX_0X_RESPECTS_UPPERCASE)
	#define ZPR_HEX_0X_RESPECTS_UPPERCASE 0
#elif (ZPR_EXPAND(ZPR_HEX_0X_RESPECTS_UPPERCASE) == 1)
	#undef ZPR_HEX_0X_RESPECTS_UPPERCASE
	#define ZPR_HEX_0X_RESPECTS_UPPERCASE 1
#endif

#if !defined(ZPR_FREESTANDING)
	#define ZPR_FREESTANDING 0
#elif (ZPR_EXPAND(ZPR_FREESTANDING) == 1)
	#undef ZPR_FREESTANDING
	#define ZPR_FREESTANDING 1
#endif

// it needs to be like this so we don't throw a redefinition warning.
#if !defined(ZPR_DECIMAL_LOOKUP_TABLE)
	#define ZPR_DECIMAL_LOOKUP_TABLE 1
#elif (ZPR_EXPAND(ZPR_DECIMAL_LOOKUP_TABLE) == 1)
	#undef ZPR_DECIMAL_LOOKUP_TABLE
	#define ZPR_DECIMAL_LOOKUP_TABLE 1
#endif

#if !defined(ZPR_HEXADECIMAL_LOOKUP_TABLE)
	#define ZPR_HEXADECIMAL_LOOKUP_TABLE 1
#elif (ZPR_EXPAND(ZPR_HEXADECIMAL_LOOKUP_TABLE) == 1)
	#undef ZPR_HEXADECIMAL_LOOKUP_TABLE
	#define ZPR_HEXADECIMAL_LOOKUP_TABLE 1
#endif

#if !defined(ZPR_USE_STD)
	#define ZPR_USE_STD 1
#elif (ZPR_EXPAND(ZPR_USE_STD) == 1)
	#undef ZPR_USE_STD
	#define ZPR_USE_STD 1
#endif


#if !ZPR_FREESTANDING
	#include <cstdio>
	#include <cstring>
#else
	#if defined(ZPR_USE_STD)
		#undef ZPR_USE_STD
	#endif

	#define ZPR_USE_STD 0

	extern "C" void* memset(void* s, int c, size_t n);
	extern "C" void* memcpy(void* dest, const void* src, size_t n);
	extern "C" void* memmove(void* dest, const void* src, size_t n);

	extern "C" size_t strlen(const char* s);
	extern "C" int strncmp(const char* s1, const char* s2, size_t n);

#endif

#if !ZPR_FREESTANDING && ZPR_USE_STD
	#include <string>
	#include <string_view>
	#include <type_traits>
#endif


#undef ZPR_DO_EXPAND
#undef ZPR_EXPAND



namespace zpr::tt
{
#if 0

	using namespace std;

#else
	template <typename T> struct type_identity { using type = T; };

	template <typename T> struct remove_reference      { using type = T; };
	template <typename T> struct remove_reference<T&>  { using type = T; };
	template <typename T> struct remove_reference<T&&> { using type = T; };

	template <typename T, T v>
	struct integral_constant
	{
		static constexpr T value = v;
		typedef T value_type;
		typedef integral_constant type;

		constexpr operator value_type() const { return value; }
		constexpr value_type operator()() const { return value; }
	};

	using true_type = integral_constant<bool, true>;
	using false_type = integral_constant<bool, false>;

	template <typename T> struct remove_cv                   { using type = T; };
	template <typename T> struct remove_cv<const T>          { using type = T; };
	template <typename T> struct remove_cv<volatile T>       { using type = T; };
	template <typename T> struct remove_cv<const volatile T> { using type = T; };

	template <typename T> using remove_cv_t = typename remove_cv<T>::type;

	template <typename> struct is_integral_base : false_type { };

	template <> struct is_integral_base<bool>               : true_type { };
	template <> struct is_integral_base<char>               : true_type { };
	template <> struct is_integral_base<signed char>        : true_type { };
	template <> struct is_integral_base<signed short>       : true_type { };
	template <> struct is_integral_base<signed int>         : true_type { };
	template <> struct is_integral_base<signed long>        : true_type { };
	template <> struct is_integral_base<signed long long>   : true_type { };
	template <> struct is_integral_base<unsigned char>      : true_type { };
	template <> struct is_integral_base<unsigned short>     : true_type { };
	template <> struct is_integral_base<unsigned int>       : true_type { };
	template <> struct is_integral_base<unsigned long>      : true_type { };
	template <> struct is_integral_base<unsigned long long> : true_type { };

	template <typename T> struct is_integral : is_integral_base<remove_cv_t<T>> { };
	template <typename T> constexpr auto is_integral_v = is_integral<T>::value;


	template <typename> struct is_signed_base : false_type { };

	template <> struct is_signed_base<signed char>        : true_type { };
	template <> struct is_signed_base<signed short>       : true_type { };
	template <> struct is_signed_base<signed int>         : true_type { };
	template <> struct is_signed_base<signed long>        : true_type { };
	template <> struct is_signed_base<signed long long>   : true_type { };

	template <typename T> struct is_signed : is_signed_base<remove_cv_t<T>> { };
	template <typename T> constexpr auto is_signed_v = is_signed<T>::value;

	template <typename T, typename U>   struct is_same : false_type { };
	template <typename T>               struct is_same<T, T> : true_type { };

	template <typename A, typename B>
	constexpr auto is_same_v = is_same<A, B>::value;

	// the 3 major compilers -- clang, gcc, and msvc -- support __is_enum. it's not
	// tenable implement is_enum without compiler magic.
	template <typename T> struct is_enum { static constexpr bool value = __is_enum(T); };
	template <typename T> constexpr auto is_enum_v = is_enum<T>::value;

	template <typename T> struct is_reference      : false_type { };
	template <typename T> struct is_reference<T&>  : true_type { };
	template <typename T> struct is_reference<T&&> : true_type { };

	template <typename T> struct is_const          : false_type { };
	template <typename T> struct is_const<const T> : true_type { };

	template <typename T> constexpr auto is_const_v = is_const<T>::value;
	template <typename T> constexpr auto is_reference_v = is_reference<T>::value;

	// a similar story exists for __underlying_type.
	template <typename T> struct underlying_type { using type = __underlying_type(T); };
	template <typename T> using underlying_type_t = typename underlying_type<T>::type;

	template <bool B, typename T = void> struct enable_if { };
	template <typename T> struct enable_if<true, T> { using type = T; };
	template <bool B, typename T = void> using enable_if_t = typename enable_if<B, T>::type;

	template <typename T> struct is_array : false_type { };
	template <typename T> struct is_array<T[]> : true_type { };
	template <typename T, size_t N> struct is_array<T[N]> : true_type { };

	template <typename T> struct remove_extent { using type = T; };
	template <typename T> struct remove_extent<T[]> { using type = T; };
	template <typename T, size_t N> struct remove_extent<T[N]> { using type = T; };

	template <typename T> struct is_function : integral_constant<bool, !is_const_v<const T> && !is_reference_v<T>> { };
	template <typename T> constexpr auto is_function_v = is_function<T>::value;

	template <typename T> auto try_add_pointer(int) -> type_identity<typename remove_reference<T>::type*>;
	template <typename T> auto try_add_pointer(...) -> type_identity<T>;

	template <typename T>
	struct add_pointer : decltype(try_add_pointer<T>(0)) { };

	template <bool B, typename T, typename F> struct conditional { using type = T; };
	template <typename T, typename F> struct conditional<false, T, F> { using type = F; };
	template <bool B, typename T, typename F> using conditional_t = typename conditional<B,T,F>::type;

	template <typename...> struct conjunction : true_type { };
	template <typename B1> struct conjunction<B1> : B1 { };
	template <typename B1, typename... Bn>
	struct conjunction<B1, Bn...> : conditional_t<bool(B1::value), conjunction<Bn...>, B1> { };

	template <typename...> struct disjunction : false_type { };
	template <typename B1> struct disjunction<B1> : B1 { };
	template <typename B1, typename... Bn>
	struct disjunction<B1, Bn...> : conditional_t<bool(B1::value), B1, disjunction<Bn...>>  { };

	template <typename B>
	struct negation : integral_constant<bool, !bool(B::value)> { };

	template <typename T>
	struct decay
	{
	private:
		using U = typename remove_reference<T>::type;
	public:
		using type = typename conditional<
			is_array<U>::value,
			typename remove_extent<U>::type*,
			typename conditional<
				is_function<U>::value,
				typename add_pointer<U>::type,
				typename remove_cv<U>::type
			>::type
		>::type;
	};

	template <typename T> using decay_t = typename decay<T>::type;

	template <typename T, bool = is_integral<T>::value>
	struct _is_unsigned : integral_constant<bool, (T(0) < T(-1))> { };

	template <typename T>
	struct _is_unsigned<T, false> : false_type { };

	template <typename T>
	struct is_unsigned : _is_unsigned<T>::type { };

	template <typename T>
	struct make_unsigned { };

	template <> struct make_unsigned<signed char> { using type = unsigned char; };
	template <> struct make_unsigned<unsigned char> { using type = unsigned char; };
	template <> struct make_unsigned<signed short> { using type = unsigned short; };
	template <> struct make_unsigned<unsigned short> { using type = unsigned short; };
	template <> struct make_unsigned<signed int> { using type = unsigned int; };
	template <> struct make_unsigned<unsigned int> { using type = unsigned int; };
	template <> struct make_unsigned<signed long> { using type = unsigned long; };
	template <> struct make_unsigned<unsigned long> { using type = unsigned long; };
	template <> struct make_unsigned<signed long long> { using type = unsigned long long; };
	template <> struct make_unsigned<unsigned long long> { using type = unsigned long long; };

	template <typename T>
	using make_unsigned_t = typename make_unsigned<T>::type;


	template <typename... Xs>
	using void_t = void;

	template <typename T>
	struct __stop_declval_eval { static constexpr bool __stop = false; };

	template <typename T, typename U = T&&>
	U __declval(int);

	template <typename T>
	T __declval(long);

	template <typename T>
	auto declval() -> decltype(__declval<T>(0))
	{
		static_assert(__stop_declval_eval<T>::__stop, "declval() must not be used!");
		return __stop_declval_eval<T>::__unknown();
	}

	template <typename T> T min(const T& a, const T& b) { return a < b ? a : b; }
	template <typename T> T max(const T& a, const T& b) { return a > b ? a : b; }
	template <typename T> T abs(const T& x) { return x < 0 ? -x : x; }

	template <typename T>
	void swap(T& t1, T& t2)
	{
		T temp = static_cast<T&&>(t1);
		t1 = static_cast<T&&>(t2);
		t2 = static_cast<T&&>(temp);
	}

#endif

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
				*(--ptr) = hex_digit(value);

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
				*(--ptr) = (value + '0');

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
					return;

				fwrite(buf, sizeof(char), ptr - buf, fd);
				written += ptr - buf;

				if(last && Newline)
					written++, fputc('\n', fd);

				ptr = buf;
			}

			FILE* fd = 0;

			char buf[Limit];
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
	size_t sprint(char* buf, size_t len, tt::str_view fmt, Args&&... args)
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
				cb("[ ]");
				return;
			}

			cb("[");
			for(auto it = begin(x);;)
			{
				detail::print_one(static_cast<Cb&&>(cb), args, *it);
				++it;

				if(it != end(x)) cb(", ");
				else             break;
			}

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





#include <set>
#include <string>
#include <vector>
#include <filesystem>
#include <type_traits>

#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

#if defined(_WIN32)

#include <io.h>

#define WIN32_LEAN_AND_MEAN 1
#define NOMINMAX            1
#include <windows.h>

static constexpr int STDIN_FILENO   = 0;
static constexpr int STDOUT_FILENO  = 1;
static constexpr int STDERR_FILENO  = 2;

using ssize_t = long long;

#if 0
LPSTR GetLastErrorAsString()
{
	// https://stackoverflow.com/questions/1387064/how-to-get-the-error-message-from-the-error-code-returned-by-getlasterror

	DWORD errorMessageId = GetLastError();
	assert(errorMessageId != 0);

	LPSTR messageBuffer = nullptr;

	DWORD size = FormatMessage(
		FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
		nullptr, errorMessageId, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		reinterpret_cast<LPSTR>(&messageBuffer), 0, nullptr);

	return messageBuffer;
}
#else
DWORD GetLastErrorAsString()
{
	// https://stackoverflow.com/questions/1387064/how-to-get-the-error-message-from-the-error-code-returned-by-getlasterror

	return GetLastError();
}
#endif

#else

#include <unistd.h>

#endif // _WIN32

// standard stuff for everybody
namespace nabs
{
	namespace fs
	{
		using namespace std::filesystem;
	}

	namespace impl
	{
		template <typename... Args>
		[[noreturn]] void int_error(Args&&... args)
		{
			zpr::fprintln(stderr, "internal error: {}", zpr::sprint(std::forward<Args>(args)...));
			exit(1);
		}

		template <typename... Args>
		void int_warn(Args&&... args)
		{
			zpr::fprintln(stderr, "internal warning: {}", zpr::sprint(std::forward<Args>(args)...));
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

			FileOpenFlags& needs_write(bool x)      { _need_write = x; return *this; }
			FileOpenFlags& should_create(bool x)    { _should_create = x; return *this; }
			FileOpenFlags& append_mode(bool x)      { _append_mode = x; return *this; }
			FileOpenFlags& truncate_mode(bool x)    { _truncate_mode = x; return *this; }
			FileOpenFlags& create_perms(int x)      { _create_perms = x; return *this; }
		};
	}

	struct CompilerFlags
	{
		std::vector<std::string> options;
		std::vector<fs::path> include_paths;
	};
}




// pipes and commands
namespace nabs
{
	namespace os
	{
		using Fd = int;
		static constexpr Fd FD_NONE = -1;

		struct PipeDes
		{
			Fd read_end;
			Fd write_end;
		};

		#if defined(_WIN32)

			using Proc = HANDLE;
			static constexpr Proc PROC_NONE = nullptr;

			static size_t PIPE_BUFFER_SIZE = 16384;

			inline ssize_t fd_read(Fd fd, void* buf, size_t len)
			{
				return static_cast<ssize_t>(_read(fd, buf, len));
			}

			inline ssize_t fd_write(Fd fd, void* buf, size_t len)
			{
				return static_cast<ssize_t>(_write(fd, buf, len));
			}

			inline PipeDes make_pipe()
			{
				if(int p[2]; _pipe(p, PIPE_BUFFER_SIZE, _O_BINARY) < 0)
				{
					impl::int_error("_pipe(): {}", strerror(errno));
				}
				else
				{
					// O_NOINHERIT is functionally CLOEXEC, so no need to fcntl it
					return PipeDes { p[0], p[1] };
				}
			}

			inline Fd open_file(const char* path, FileOpenFlags fof)
			{
				int flags = 0;

				if(fof._need_write)         flags |= _O_RDWR;
				else                        flags |= _O_RDONLY;

				if(fof._should_create)      flags |= _O_CREAT;
				if(fof._truncate_mode)      flags |= _O_TRUNC;
				else if(fof._append_mode)   flags |= _O_APPEND;

				int fd = 0;
				if(fof._should_create)
					fd = _open(path, flags, _S_IREAD | _S_IWRITE);

				else
					fd = _open(path, flags);

				if(fd < 0)
					impl::int_error("open('{}'): {}", path, strerror(errno));

				return fd;
			}

			inline int wait_for_pid(Proc proc)
			{
				auto result = WaitForSingleObject(proc, INFINITE);
				if(result == WAIT_FAILED)
					impl::int_error("WaitForSingleObject(): {}", GetLastErrorAsString());

				DWORD status = 0;
				if(!GetExitCodeProcess(proc, &status))
					impl::int_error("GetExitCodeProcess(): {}", GetLastErrorAsString());

				CloseHandle(proc);
				return static_cast<int>(status);
			}

		#else

			using Fd = int;
			static constexpr Fd FD_NONE = -1;

			using Proc = pid_t;
			static constexpr Proc PROC_NONE = -1;

			inline ssize_t fd_read(Fd fd, void* buf, size_t len)
			{
				return read(fd, buf, len);
			}

			inline ssize_t fd_write(Fd fd, void* buf, size_t len)
			{
				return write(fd, buf, len);
			}

			inline PipeDes make_pipe()
			{
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
			}

			inline Fd open_file(const char* path, FileOpenFlags fof)
			{
				int flags = 0;

				if(fof._need_write)         flags |= O_RDWR;
				else                        flags |= O_RDONLY;

				if(fof._should_create)      flags |= O_CREAT;
				if(fof._truncate_mode)      flags |= O_TRUNC;
				else if(fof._append_mode)   flags |= O_APPEND;

				int fd = 0;
				if(fof._should_create)
					fd = open(path, flags, fof.create_perms);

				else
					fd = open(path, flags);

				if(fd < 0)
					impl::int_error("open('{}'): {}", path, strerror(errno));

				return fd;
			}

			inline int wait_for_pid(Proc proc)
			{
				int status = 0;
				if(waitpid(proc, &status, 0) < 0)
					impl::int_error("waitpid({}): {}", proc, strerror(errno));

				return status;
			}

		#endif // _WIN32
	}

	namespace impl
	{
		inline void split_program(std::vector<os::Fd> fds);

		struct Part;

		struct Pipeline
		{
			Pipeline(std::vector<Part> parts) : parts(std::move(parts)) { }

			bool empty() const { return this->parts.empty(); }

			int run();
			std::vector<os::Proc> runAsync();

			std::vector<Part> parts;

			friend struct Part;

		private:
			std::vector<os::Proc> runAsync(os::Fd in_fd);
		};

		struct Part
		{
			Part(Part&&) = default;
			Part(const Part&) = default;

			Part& operator= (Part&&) = default;
			Part& operator= (const Part&) = default;

			int run(os::Fd in_fd, os::Fd out_fd, os::Fd err_fd);
			os::Proc runAsync(os::Fd in_fd, os::Fd out_fd, os::Fd err_fd);

		#if defined(_WIN32)

			std::string make_args()
			{
				std::string ret;

				auto quote_thing = [](const std::string& s) -> std::string {
					if(s.find_first_of(" \t\n\v\f") == std::string::npos)
						return s;

					std::string ret = "\"";
					int backs = 0;
					for(size_t i = 0; i < s.size(); i++)
					{
						char c = s[i];

						if(c == '\\')
						{
							backs++;
							continue;
						}

						if(i == s.size() - 1)
						{
							ret.append(backs * 2, '\\');
						}
						else if(c == '"')
						{
							ret.append(backs * 2, '\\');
							ret += '"';
						}
						else
						{
							ret.append(backs, '\\');
							ret += c;
						}

						backs = 0;
					}

					ret += '"';
					return ret;
				};


				ret += quote_thing(this->name);
				for(auto& arg : this->arguments)
				{
					ret += " ";
					ret += quote_thing(arg);
				}

				return ret;
			}

		#else
			char** make_args()
			{
				char** args_array = new char*[this->arguments.size() + 2];
				for(size_t i = 0; i < arguments.size(); i++)
					args_array[1 + i] = const_cast<char*>(this->arguments[i].c_str());

				args_array[0] = const_cast<char*>(this->name.c_str());
				args_array[this->arguments.size() + 1] = nullptr;
				return args_array;
			}
		#endif // _WIN32

			static constexpr int TYPE_PROC  = 1;
			static constexpr int TYPE_FILE  = 2;
			static constexpr int TYPE_SPLIT = 3;

			int type() const  { return this->_type; }
			int flags() const { return this->_flags; }

			// used to check... certain things.
			bool is_passive() const { return this->type() == TYPE_FILE; }

			static Part of_command(std::string name, std::vector<std::string> args = { })
			{
				return Part(TYPE_PROC, 0, std::move(name), std::move(args));
			}

			static Part of_file(std::string name)
			{
				return Part(TYPE_FILE, 0, std::move(name), { });
			}

			static Part of_split(std::vector<impl::Pipeline> splits, std::vector<impl::Pipeline*> ptrs)
			{
				auto ret = Part(TYPE_SPLIT, 0, "split", { });
				ret.split_vals = std::move(splits);
				ret.split_ptrs = std::move(ptrs);
				return ret;
			}

		private:
			Part(int type, int flags, std::string name, std::vector<std::string> args)
				: _type(type)
				, _flags(flags)
				, name(std::move(name))
				, arguments(std::move(args))
			{ }

			int _type;
			int _flags;
			std::string name;
			std::vector<std::string> arguments;

			std::vector<impl::Pipeline> split_vals;
			std::vector<impl::Pipeline*> split_ptrs;

			friend struct Pipeline;
		};

		// ADL should let us find this operator.
		inline Pipeline operator| (Pipeline head, const Pipeline& tail)
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


		os::Proc Part::runAsync(os::Fd in_fd, os::Fd out_fd, os::Fd err_fd)
		{
			auto dupe_fd = [](int src, int dst, bool close_src = true) {
				// dup2 closes dst for us.
				if(dup2(src, dst) < 0)
					int_error("dup2({}, {}): {}", src, dst, strerror(errno));

				if(close_src)
					close(src);
			};

			if(this->type() == TYPE_PROC)
			{
			#if defined(_WIN32)

				auto get_handle = [](os::Fd fd, HANDLE def) -> HANDLE {
					if(fd == os::FD_NONE)
						return def;

					else
						return reinterpret_cast<HANDLE>(_get_osfhandle(fd));
				};


				STARTUPINFO info;
				memset(&info, 0, sizeof(info));

				info.cb = sizeof(STARTUPINFO);
				info.hStdInput  = get_handle(in_fd, GetStdHandle(STD_INPUT_HANDLE));
				info.hStdOutput = get_handle(out_fd, GetStdHandle(STD_OUTPUT_HANDLE));
				info.hStdError  = get_handle(err_fd, GetStdHandle(STD_ERROR_HANDLE));
				info.dwFlags |= STARTF_USESTDHANDLES;

				PROCESS_INFORMATION procinfo;
				memset(&procinfo, 0, sizeof(procinfo));

				auto cmdline = this->make_args();
				auto cmdline_ = const_cast<LPSTR>(cmdline.c_str());

				auto result = CreateProcessA(
					nullptr,    // LPCSTR                   lpApplicationName
					cmdline_,   // LPSTR                    lpCommandLine
					nullptr,    // LPSECURITY_ATTRIBUTES    lpProcessAttributes
					nullptr,    // LPSECURITY_ATTRIBUTES    lpThreadAttributes
					true,       // BOOL                     bInheritHandles
					0,          // DWORD                    dwCreationFlags
					nullptr,    // LPVOID                   lpEnvironment
					nullptr,    // LPCSTR                   lpCurrentDirectory
					&info,      // LPSTARTUPINFO            lpStartupInfo
					&procinfo   // LPPROCESS_INFORMATION    lpProcessInformation
				);

				if(!result)
					impl::int_error("CreateProcess('{}'): {}", cmdline, GetLastErrorAsString());

				CloseHandle(procinfo.hThread);
				return procinfo.hProcess;

			#else
				if(auto child = fork(); child < 0)
				{
					int_error("fork(): {}", strerror(errno));
				}
				else if(child == 0)
				{
					if(in_fd != os::FD_NONE)
					{
						dupe_fd(in_fd, STDIN_FILENO);
					}
					else
					{
						// TODO: make closing stdin configurable
						// for now, we leave stdin open. this might be a problem later,
						// and we definitely want to make it configurable later on.
					}

					if(out_fd != os::FD_NONE)
						dupe_fd(out_fd, STDOUT_FILENO);

					if(err_fd != os::FD_NONE)
						dupe_fd(err_fd, STDERR_FILENO);

					if(execvp(this->name.c_str(), this->make_args()) < 0)
						int_error("execvp('{}'): {}", this->name, strerror(errno));

					abort();
				}
				else
				{
					return child;
				}
			#endif // _WIN32
			}
			else if(this->type() == TYPE_FILE)
			{
				// if the in_fd is not -1, then we need to write to the file.
				os::Fd file = os::open_file(this->name.c_str(), os::FileOpenFlags()
					.needs_write(in_fd != os::FD_NONE)

					// TODO: make a way to specify the file opening flags/modes
					.append_mode(true)      // append by default
					.should_create(true)    // create by default
					.create_perms(0664)
				);

				if(in_fd != os::FD_NONE)
					dupe_fd(file, in_fd, /* close_src: */ false);

				if(out_fd != os::FD_NONE)
					dupe_fd(file, out_fd, /* close_src: */ false);

				close(file);
				return os::PROC_NONE;
			}
			else if(this->type() == TYPE_SPLIT)
			{

			#if defined(_WIN32)

				int_error("split() not supported on windows yet");
				return os::PROC_NONE;

			#else
				// basically we have to emulate what the `tee` program does.
				if(auto child = fork(); child < 0)
				{
					int_error("fork(): {}", strerror(errno));
				}
				else if(child == 0)
				{
					if(in_fd == os::FD_NONE)
						int_error("tee() cannot be the first item in a pipeline");

					std::vector<os::Fd> fds;
					std::vector<os::Proc> children;

					// it's basically like a normal process
					if(in_fd != os::FD_NONE)
						dupe_fd(in_fd, STDIN_FILENO);

					if(out_fd != os::FD_NONE)
						dupe_fd(out_fd, STDOUT_FILENO);

					if(err_fd != os::FD_NONE)
						dupe_fd(err_fd, STDERR_FILENO);

					auto iterate_splits = [&fds, &children](Pipeline* pl) {

						assert(pl != nullptr);
						auto [ p_read, p_write ] = os::make_pipe();

						auto cs = pl->runAsync(p_read);
						children.insert(children.end(), cs.begin(), cs.end());

						// if the pipeline's first argument is passive, then we write directly into it.
						// we rely on the pipeline's run() to perform dup2() to change our fd as appropriate.
						if(pl->parts[0].is_passive())
						{
							close(p_write);
							fds.push_back(p_read);
						}
						else
						{
							close(p_read);
							fds.push_back(p_write);
						}
					};

					if(!this->split_ptrs.empty())
					{
						for(auto& sp : this->split_ptrs)
							iterate_splits(sp);
					}
					else
					{
						for(auto& sp : this->split_vals)
							iterate_splits(&sp);
					}

					split_program(std::move(fds));

					// wait for all the children...
					for(auto c : children)
						os::wait_for_pid(c);

					exit(0);
				}
				else
				{
					return child;
				}
			#endif
			}
			else
			{
				int_error("unknown pipeline component type '{}'", this->type());
			}
		}

		int Part::run(os::Fd in_fd, os::Fd out_fd, os::Fd err_fd)
		{
			auto child_pid = this->runAsync(in_fd, out_fd, err_fd);
			if(child_pid == os::PROC_NONE)
				return 0;

			return os::wait_for_pid(child_pid);
		}

		std::vector<os::Proc> Pipeline::runAsync(os::Fd in_fd)
		{
			std::vector<os::Proc> children;
			os::Fd predecessor_pipe = in_fd;

			if(this->empty())
				int_error("unexpected empty pipeline");

			size_t start = 0;
			if(!this->parts.empty() && this->parts[0].type() == Part::TYPE_FILE)
			{
				if(this->parts.size() > 1)
				{
					// in this case, we should not have an `in_fd`, since that would conflict
					// with the file.
					if(in_fd != os::FD_NONE)
						int_error("file() must be the last item in a tee()");

					// open this file in read mode.
					os::Fd file = os::open_file(this->parts[0].name.c_str(),
						os::FileOpenFlags().needs_write(false).should_create(false)
					);

					predecessor_pipe = file;
					start = 1;
				}
				else
				{
					// open this file as write mode.
					// now, we just need to dup2 the file descriptor to the in_fd, and return immediately.
					// by passing in_fd, we get the file to open in write mode.
					this->parts[0].runAsync(in_fd, os::FD_NONE, os::FD_NONE);
					return { };
				}
			}

			for(size_t i = start; i < this->parts.size(); i++)
			{
				bool did_file = false;
				bool is_last = (i == this->parts.size() - 1);

				auto [ pipe_read, pipe_write ] = os::make_pipe();

				// we need some special handling here if the output of this stage goes to a file.
				// at the point of the fork, the parent (us) won't have the file opened, so even if
				// we play with dup2() etc, the child will never be able to see the file. so, we need
				// to scan-ahead and see if the next idiot is a file, and if so we must open it now.
				if(!is_last && this->parts[i + 1].type() == Part::TYPE_FILE)
				{
					// 'run' the file (ie. setup the fds); the input to the file is the write end
					// of *THIS* process's Pipeline (ie. output) -- so writing to it will write to the file,
					// and the 'output' of the file will be the input of the *next* process -- so reading from
					// it reads from the file.
					this->parts[i + 1].runAsync(pipe_write, pipe_read, os::FD_NONE);
					did_file = true;

					// if this is not the last component, abort also.
					if(i + 2 != this->parts.size())
						int_error("unexpected file ('{}') in the middle of a pipeline", this->parts[i + 1].name);
				}

				if(is_last)
				{
					close(pipe_write);
					pipe_write = os::FD_NONE;
				}

				// someone else should have caught this already.
				assert(this->parts[i].type() != Part::TYPE_FILE);

				// TODO: stderr is not handled here
				// we don't really know how to link them together from proc to proc.
				auto child = this->parts[i].runAsync(predecessor_pipe, pipe_write, os::FD_NONE);
				if(child != os::PROC_NONE)
					children.push_back(child);

				// setup the read end for the next process.
				if(predecessor_pipe != os::FD_NONE)
					close(predecessor_pipe);

				predecessor_pipe = pipe_read;
				if(!is_last)
					close(pipe_write);

				if(did_file)
					i += 1;
			}

			close(predecessor_pipe);
			return children;
		}

		std::vector<os::Proc> Pipeline::runAsync()
		{
			return this->runAsync(os::FD_NONE);
		}

		int Pipeline::run()
		{
			auto children = this->runAsync();

			int status = 0;
			for(auto c : children)
				status = os::wait_for_pid(c);

			// just use the last one.
			return status;
		}

		inline void split_program(std::vector<os::Fd> fds)
		{
			char buf[4096] { };
			while(true)
			{
				auto n = os::fd_read(STDIN_FILENO, buf, 4096);
				if(n <= 0)
				{
					if(n < 0) zpr::fprintln(stderr, "tee(stdin): read error: {}", strerror(errno));
					break;
				}

				if(os::fd_write(STDOUT_FILENO, buf, n) < 0)
					zpr::fprintln(stderr, "tee(stdout): write error: {}", strerror(errno));

				for(auto& f : fds)
				{
					if(os::fd_write(f, buf, n) < 0)
						zpr::fprintln(stderr, "tee({}): write error: {}", f, strerror(errno));
				}
			}

			for(auto& f : fds)
				close(f);
		}
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

	template <typename... Args>
	inline impl::Pipeline tee(Args&&... args)
	{
		static_assert(((std::is_convertible_v<Args, fs::path>) && ...),
			"tee() requires std::filesystem::path");

		return impl::Pipeline({
			impl::Part::of_split(std::vector<impl::Pipeline> { nabs::file(args)... }, { })
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
}



// compiler specifics
namespace nabs
{
	struct Compiler
	{
		fs::path path;
		int kind;
		int lang;

		static constexpr int KIND_GCC_CLANG = 1;
		static constexpr int KIND_MSVC_CL   = 2;

		static constexpr int LANG_C         = 1;
		static constexpr int LANG_CPP       = 2;
	};

	namespace impl
	{
		inline std::vector<fs::path> get_path_variable()
		{
			std::vector<fs::path> ret;
			std::string var = getenv("PATH");

			while(true)
			{
				auto i = var.find(':');
				if(i == std::string::npos)
				{
					ret.push_back(var);
					break;
				}
				else
				{
					auto comp = var.substr(0, i);
					ret.push_back(comp);
					var = var.substr(i + 1);
				}
			}

			return ret;
		}

		inline fs::path find_file_in_path(const fs::path& file, const std::vector<fs::path>& path)
		{
			for(auto& p : path)
			{
				auto tmp = (p / file);
				if(fs::exists(tmp))
					return tmp;
			}

			return { };
		}
	}

	inline Compiler find_c_compiler()
	{
		// TODO: support cross-compilation better
		// basically we need a way to define the target, provide a sysroot, and
		// find a specific "brand" of compiler, if you will.

		// for now, we just use the platform #defines to see which compiler to
		// search for. if we see _WIN32 but nothing suggesting cygwin or mingw,
		// then use MSVC's cl.exe. else, just use `cc` like a unix system.

	#if defined(_WIN32) && !defined(__CYGWIN__) && !defined(__MINGW32__) && !defined(__MINGW64__)

		impl::int_error("msvc is not supported yet");

	#else
		// basically, find 'cc' in the path.
		auto path_env = impl::get_path_variable();
		if(auto clang = impl::find_file_in_path("clang", path_env); !clang.empty())
			return { .path = clang, .kind = Compiler::KIND_GCC_CLANG, .lang = Compiler::LANG_C };

		else if(auto gcc = impl::find_file_in_path("gcc", path_env); !gcc.empty())
			return { .path = gcc, .kind = Compiler::KIND_GCC_CLANG, .lang = Compiler::LANG_C };

		else if(auto cc = impl::find_file_in_path("cc", path_env); !cc.empty())
			return { .path = cc, .kind = Compiler::KIND_GCC_CLANG, .lang = Compiler::LANG_C };

		else
			impl::int_error("could not find any C compiler in the $PATH");

	#endif // _WIN32
	}

	inline Compiler find_cpp_compiler()
	{
	#if defined(_WIN32) && !defined(__CYGWIN__) && !defined(__MINGW32__) && !defined(__MINGW64__)

		// msvc uses cl.exe for both C and C++ -- the hard part is actually finding the damn thing.
		return find_c_compiler();

	#else
		// basically, find 'c++' in the path.
		auto path_env = impl::get_path_variable();
		if(auto clang = impl::find_file_in_path("clang++", path_env); !clang.empty())
			return { .path = clang, .kind = Compiler::KIND_GCC_CLANG, .lang = Compiler::LANG_CPP };

		else if(auto cc = impl::find_file_in_path("c++", path_env); !cc.empty())
			return { .path = cc, .kind = Compiler::KIND_GCC_CLANG, .lang = Compiler::LANG_CPP };

		else if(auto gcc = impl::find_file_in_path("g++", path_env); !gcc.empty())
			return { .path = gcc, .kind = Compiler::KIND_GCC_CLANG, .lang = Compiler::LANG_CPP };

		else
			impl::int_error("could not find any C++ compiler in the $PATH");

	#endif // _WIN32
	}
}




// actual compiling
namespace nabs
{
	inline bool compile_files(const CompilerFlags& opts, const fs::path& output, const std::vector<fs::path>& files)
	{
		bool is_cpp = false;
		for(auto& p : files)
		{
			auto ext = p.extension();
			if(ext == ".cpp" || ext == ".cc" || ext == ".cxx" || ext == ".c++")
			{
				is_cpp = true;
				break;
			}
		}

		std::vector<std::string> args = opts.options;
		auto cmp = is_cpp
			? find_cpp_compiler()
			: find_c_compiler();

		if(cmp.kind == Compiler::KIND_GCC_CLANG)
		{
			for(auto& inc : opts.include_paths)
				args.push_back(zpr::sprint("-I{}", inc.string()));

			// TODO: need a way to specify default c/c++ standard
			if(cmp.lang == Compiler::LANG_CPP)      args.push_back("-std=c++17");
			else if(cmp.lang == Compiler::LANG_C)   args.push_back("-std=c11");

			args.push_back("-o");
			args.push_back(output.string());
		}
		else
		{
			impl::int_error("unsupported compiler");
		}

		// assume all compilers let you just throw the input files at the end with no regard.
		for(auto& f : files)
			args.push_back(f.string());

		// construct the command, and run it.
		return cmd(cmp.path.string(), std::move(args)).run();
	}

	template <typename... Args, typename = std::enable_if_t<((std::is_convertible_v<Args, fs::path>) && ...)>>
	inline bool compile_files(const CompilerFlags& opts, const fs::path& output, Args&&... inputs)
	{
		return compile_files(opts, output, std::vector<fs::path> { inputs... });
	}
}



// dependency stuff
namespace nabs::dep
{
	struct Item
	{
		void depend(Item item)
		{
			if(std::find(this->deps.begin(), this->deps.end(), item) != this->deps.end())
			{
				impl::int_warn("'{}' already depends on '{}'", this->str(), item.str());
				return;
			}

			this->deps.push_back(std::move(item));
		}

		// two items are equal if their names/paths are the same.
		// dependencies are NOT considered in equality checks.
		bool operator== (const Item& other) const
		{
			if(this->phony != other.phony)
				return false;

			if(this->phony)
				return this->name == other.name;

			else
				return fs::equivalent(this->path, other.path);
		}

		bool operator!= (const Item& item) const
		{
			return !(*this == item);
		}

		bool operator< (const Item& item) const
		{
			return this->str() < item.str();
		}

		std::string str() const
		{
			if(this->phony)
				return this->name;

			else
				return this->path.string();
		}

		fs::path path;

		std::string name;
		bool phony = false;

		std::vector<Item> deps;

		Item() { }
		Item(bool phony, std::string name, fs::path path)
			: path(std::move(path))
			, name(std::move(name))
			, phony(phony)
		{ }
	};

	inline Item create(fs::path path)
	{
		return Item(/* phony: */ false, "", std::move(path));
	}

	inline Item create_phony(std::string name)
	{
		return Item(/* phony: */ true, std::move(name), "");
	}

	namespace impl
	{
		inline void topological_sort(const Item& root, std::vector<std::vector<Item>>& result, std::set<Item>& seen)
		{
			if(seen.find(root) != seen.end())
				nabs::impl::int_error("circular dependency: '{}'", root.str());

			seen.insert(root);
			std::vector<Item> current;
			for(auto& dep : root.deps)
			{
				current.push_back(dep);
				topological_sort(dep, result, seen);
			}

			result.push_back(std::move(current));
		}
	}

	inline std::vector<std::vector<Item>> topological_sort(const Item& root)
	{
		std::vector<std::vector<Item>> ret;
		ret.push_back({ root });

		std::set<Item> seen;
		impl::topological_sort(root, ret, seen);

		std::reverse(ret.begin(), ret.end());
		return ret;
	}
}




// filesystem stuff
namespace nabs::fs
{
	// note that std::filesystem has been 'used' here,
	// but it is suggested to still prefix fs:: to things that come from std and
	// not from us.

	namespace impl
	{
		template <typename Iter, typename Predicate>
		inline std::vector<fs::path> find_files_helper(const fs::path& dir, Predicate&& pred)
		{
			// i guess this is not an error...?
			if(!fs::is_directory(dir))
				return { };

			std::vector<fs::path> paths;
			auto iter = Iter(dir);
			for(auto& ent : iter)
			{
				if((ent.is_regular_file() || ent.is_symlink()) && pred(ent))
					paths.push_back(ent.path());
			}

			return paths;
		}
	}

	template <typename Predicate>
	inline std::vector<fs::path> find_files(const fs::path& dir, Predicate&& pred)
	{
		return impl::find_files_helper<fs::directory_iterator>(dir, pred);
	}

	template <typename Predicate>
	inline std::vector<fs::path> find_files_recursively(const fs::path& dir, Predicate&& pred)
	{
		return impl::find_files_helper<fs::recursive_directory_iterator>(dir, pred);
	}
}






