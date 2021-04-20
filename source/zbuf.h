/*
	zbuf.h
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
	Version 1.0.4
	=============


	Documentation
	=============

	This is a freestanding library that can be compiled without any libc or STL headers, apart from
	freestanding headers (stdint.h and stddef.h).

	A simple and lightweight library implementing 'stretchy buffers' and immutable views of them, akin
	to std::vector<T> and std::span<T>, but operating purely on the basis of bytes. The intended usecase
	for this library is dealing with data representation and exchange as byte arrays.

	A lightweight replacement for std::string_view is also included (str_view) that has a more ergonomic
	API, namely drop(), drop_last(), take(), and take_last() that return copies, instead of being limited
	to remove_prefix() and remove_suffix().

	There are optional #define macros to control behaviour; defining them without a value constitues
	a TRUE -- for FALSE, either `#undef` them, or explicitly define them as 0. For values that are TRUE
	by default, you *must* explicitly `#define` them as 0.

	- ZBUF_USE_STD
		this is *TRUE* by default. controls whether or not STL type interfaces are used; with it,
		str_view gains implicit constructors accepting std::string and std::string_view, as well
		as methods to convert to them (sv() and str()).

		Also, Buffer and Span get fromString() methods.


	- ZBUF_FREESTANDING
		this is *FALSE by default; controls whether or not a standard library implementation is
		available. if not, then memset(), memmove(), strlen(), and strncmp() are forward declared according to the
		C library specifications, but not defined.


	Note that ZBUF_FREESTANDING implies ZBUF_USE_STD = 0.



	Version History
	===============

	1.0.4 - 30/03/2021
	------------------
	Add '.back()' and '.front()' for str_view



	1.0.3 - 17/03/2021
	------------------
	Bug fixes:
	- fix completely broken comparison for str_view



	1.0.2 - 16/03/2021
	------------------
	Bug fixes:
	- fix ambiguous (read: unnecessary) operator== and operator!= for str_view



	1.0.1 - 16/03/2021
	------------------
	Bug fixes:
	- make begin() and end() for str_view inline



	1.0.0 - 14/03/2021
	------------------
	Initial release.
*/



#pragma once

#define ZBUF_DO_EXPAND(VAL)  VAL ## 1
#define ZBUF_EXPAND(VAL)     ZBUF_DO_EXPAND(VAL)

#if !defined(ZBUF_USE_STD)
	#define ZBUF_USE_STD 1
#elif (ZBUF_EXPAND(ZBUF_USE_STD) == 1)
	#undef ZBUF_USE_STD
	#define ZBUF_USE_STD 1
#endif

#if !defined(ZBUF_FREESTANDING)
	#define ZBUF_FREESTANDING 0
#elif (ZBUF_EXPAND(ZBUF_FREESTANDING) == 1)
	#undef ZBUF_FREESTANDING
	#define ZBUF_FREESTANDING 1
#endif


#if !ZBUF_FREESTANDING
	#include <cstring>
#else
	#undef ZBUF_USE_STD
	#define ZBUF_USE_STD 0

	extern "C" void* memset(void* s, int c, size_t n);
	extern "C" void* memmove(void* dest, const void* src, size_t n);

	extern "C" size_t strlen(const char* s);
	extern "C" int strncmp(const char* s1, const char* s2, size_t n);
#endif

#if !ZBUF_FREESTANDING && ZBUF_USE_STD
	#include <string>
	#include <string_view>
#endif

#undef ZBUF_DO_EXPAND
#undef ZBUF_EXPAND

#include <cstdint>
#include <cstddef>

namespace zbuf
{
	namespace detail
	{
		template <typename T> T min(T a, T b) { return a < b ? a : b;}

		template <typename, typename>   struct is_same { static constexpr bool value = false; };
		template <typename T>           struct is_same<T, T> { static constexpr bool value = true; };


		template <bool B, typename T = void>    struct enable_if { };
		template <typename T>                   struct enable_if<true, T> { using type = T; };
	}

	struct Span;

	struct str_view
	{
		using value_type = char;

		str_view() : ptr(nullptr), len(0) { }
		str_view(const char* p, size_t l) : ptr(p), len(l) { }

		template <size_t N>
		str_view(const char (&s)[N]) : ptr(s), len(N - 1) { }

		template <typename T, typename = typename detail::enable_if<detail::is_same<const char*, T>::value>::type>
		str_view(T s) : ptr(s), len(strlen(s)) { }

		str_view(str_view&&) = default;
		str_view(const str_view&) = default;
		str_view& operator= (str_view&&) = default;
		str_view& operator= (const str_view&) = default;

		inline bool operator== (const str_view& other) const
		{
			return (this->len == other.len) &&
				(this->ptr == other.ptr || (strncmp(this->ptr, other.ptr, detail::min(this->len, other.len)) == 0)
			);
		}

		inline bool operator!= (const str_view& other) const
		{
			return !(*this == other);
		}

		inline const char* begin() const { return this->ptr; }
		inline const char* end() const { return this->ptr + len; }

		inline size_t length() const { return this->len; }
		inline size_t size() const { return this->len; }
		inline bool empty() const { return this->len == 0; }
		inline const char* data() const { return this->ptr; }

		char front() const { return this->ptr[0]; }
		char back() const { return this->ptr[this->len - 1]; }

		inline void clear() { this->ptr = 0; this->len = 0; }

		inline char operator[] (size_t n) { return this->ptr[n]; }

		inline size_t find(char c) const { return this->find(str_view(&c, 1)); }
		inline size_t find(str_view sv) const
		{
			if(sv.size() > this->size())
				return -1;

			else if(sv.empty())
				return 0;

			for(size_t i = 0; i < 1 + this->size() - sv.size(); i++)
			{
				if(this->drop(i).take(sv.size()) == sv)
					return i;
			}

			return -1;
		}

		inline size_t rfind(char c) const { return this->rfind(str_view(&c, 1)); }
		inline size_t rfind(str_view sv) const
		{
			if(sv.size() > this->size())
				return -1;

			else if(sv.empty())
				return this->size() - 1;

			for(size_t i = 1 + this->size() - sv.size(); i-- > 0;)
			{
				if(this->take(1 + i).take_last(sv.size()) == sv)
					return i;
			}

			return -1;
		}

		inline size_t find_first_of(str_view sv)
		{
			for(size_t i = 0; i < this->len; i++)
			{
				for(char k : sv)
				{
					if(this->ptr[i] == k)
						return i;
				}
			}

			return -1;
		}

		inline str_view drop(size_t n) const { return (this->size() >= n ? this->substr(n, this->size() - n) : ""); }
		inline str_view take(size_t n) const { return (this->size() >= n ? this->substr(0, n) : *this); }
		inline str_view take_last(size_t n) const { return (this->size() >= n ? this->substr(this->size() - n, n) : *this); }
		inline str_view drop_last(size_t n) const { return (this->size() >= n ? this->substr(0, this->size() - n) : *this); }

		inline str_view substr(size_t pos = 0, size_t cnt = -1) const
		{
			return str_view(this->ptr + pos, cnt == static_cast<size_t>(-1)
				? (this->len > pos ? this->len - pos : 0)
				: cnt);
		}

		inline str_view& remove_prefix(size_t n) { return (*this = this->drop(n)); }
		inline str_view& remove_suffix(size_t n) { return (*this = this->drop_last(n)); }

		inline Span span() const;

	#if ZBUF_USE_STD
		str_view(const std::string& str) : ptr(str.data()), len(str.size()) { }
		str_view(const std::string_view& sv) : ptr(sv.data()), len(sv.size()) { }

		inline std::string_view sv() const  { return std::string_view(this->data(), this->size()); }
		inline std::string str() const      { return std::string(this->data(), this->size()); }
	#endif // ZBUF_USE_STD

	private:
		const char* ptr;
		size_t len;
	};

	#if ZBUF_USE_STD
		inline bool operator== (const std::string& other, str_view sv) { return sv == str_view(other); }
		inline bool operator!= (std::string_view other, str_view sv) { return sv != str_view(other); }
	#endif // ZBUF_USE_STD


	inline const char* begin(const str_view& sv) { return sv.begin(); }
	inline const char* end(const str_view& sv) { return sv.end(); }


	struct Buffer
	{
		Buffer(const Buffer&) = delete;
		Buffer& operator= (const Buffer&) = delete;


		inline Buffer() : Buffer(64)
		{
		}

		inline ~Buffer()
		{
			if(this->ptr)
				delete[] this->ptr;
		}

		inline explicit Buffer(size_t cap) : len(0), cap(cap), ptr(new uint8_t[cap])
		{
			this->originalCap = cap;
		}

		inline Buffer(Buffer&& oth)
		{
			this->ptr = oth.ptr;    oth.ptr = nullptr;
			this->len = oth.len;    oth.len = 0;
			this->cap = oth.cap;    oth.cap = 0;
			this->originalCap = oth.originalCap;
		}

		inline Buffer& operator= (Buffer&& oth)
		{
			if(this == &oth)
				return *this;

			if(this->ptr)
				delete[] this->ptr;

			this->ptr = oth.ptr;    oth.ptr = nullptr;
			this->len = oth.len;    oth.len = 0;
			this->cap = oth.cap;    oth.cap = 0;
			this->originalCap = oth.originalCap;

			return *this;
		}

		template <typename T>
		inline T* as()
		{
			return reinterpret_cast<T*>(this->ptr);
		}

		template <typename T>
		inline const T* as() const
		{
			return reinterpret_cast<const T*>(this->ptr);
		}

		inline Span span() const;
		inline str_view sv() const { return str_view(reinterpret_cast<const char*>(this->ptr), this->len); }

		inline Buffer clone() const
		{
			auto ret = Buffer(this->cap);
			ret.write(this->ptr, this->len);

			return ret;
		}

		inline uint8_t* data()             { return this->ptr; }
		inline const uint8_t* data() const { return this->ptr; }

		inline size_t size() const         { return this->len; }
		inline bool full() const           { return this->len == this->cap; }
		inline size_t remaining() const    { return this->cap - this->len; }

		// use this when you wrote data to the buffer via external means, and now
		// want to update its size.
		inline void incrementSize(size_t by)
		{
			this->len += by;
		}

		inline void clear()
		{
			memset(this->ptr, 0, this->len);
			this->len = 0;
		}

		inline void unsafeClear()
		{
			this->len = 0;
		}

		inline void reset()
		{
			if(this->cap != this->originalCap)
			{
				delete[] this->ptr;
				this->cap = this->originalCap;
				this->len = 0;
				this->ptr = new uint8_t[this->cap];
			}
			else
			{
				this->clear();
			}
		}

		// this is NOT a fast operation!!
		inline Buffer& drop(size_t n)
		{
			if(n >= this->len)
			{
				this->len = 0;
			}
			else if(n > 0)
			{
				memmove(this->ptr, this->ptr + n, this->len - n);
			}

			return *this;
		}

		inline size_t write(Span s);
		inline void autoWrite(Span s);

		inline size_t write(str_view sv) { return this->write(sv.data(), sv.size()); }
		inline size_t write(const Buffer& b) { return this->write(b.data(), b.size());}
		inline size_t write(const void* data, size_t len)
		{
			auto todo = detail::min(len, this->cap - this->len);

			memmove(this->ptr + this->len, data, todo);
			this->len += todo;

			return todo;
		}

		inline void autoWrite(str_view sv) { return this->autoWrite(sv.data(), sv.size());}
		inline void autoWrite(const Buffer& b) { return this->autoWrite(b.data(), b.size());}
		inline void autoWrite(const void* data, size_t len)
		{
			if(this->remaining() < len)
				this->grow(len - this->remaining());

			this->write(data, len);
		}


		// auto-expands by 1.6x
		inline void grow()
		{
			this->resize(this->cap * 1.6);
		}

		// expands only by the specified amount
		inline void grow(size_t sz)
		{
			this->resize(this->cap + sz);
		}

		// changes the size to be sz. (only expands, never contracts)
		inline void resize(size_t sz)
		{
			if(sz < this->cap)
				return;

			auto tmp = new uint8_t[sz];
			if(this->ptr)
			{
				memmove(tmp, this->ptr, this->len);
				delete[] this->ptr;
			}

			this->ptr = tmp;
			this->cap = sz;
		}


		static inline Buffer empty()
		{
			return Buffer(0);
		}

	#if ZBUF_USE_STD
		static inline Buffer fromString(std::string_view sv)
		{
			auto r = Buffer(sv.size());
			r.write(sv.data(), sv.size());
			return r;
		}

		static inline Buffer fromString(const std::string& str)
		{
			auto r = Buffer(str.size());
			r.write(str.data(), str.size());
			return r;
		}
	#endif // ZBUF_USE_STD


	private:
		size_t len;
		size_t cap;
		uint8_t* ptr;

		size_t originalCap;
	};

	struct Span
	{
		inline Span(const char* p, size_t l) : ptr(reinterpret_cast<const uint8_t*>(p)), len(l) { }
		inline Span(const uint8_t* p, size_t l) : ptr(p), len(l) { }

		inline Span(Span&&) = default;
		inline Span(const Span&) = default;
		inline Span& operator = (Span&&) = default;
		inline Span& operator = (const Span&) = default;

		inline Buffer reify() const
		{
			auto ret = Buffer(this->len);
			ret.write(this->ptr, this->len);

			return ret;
		}

		inline size_t size() const { return this->len; }
		inline const uint8_t* data() const { return this->ptr; }
		inline zbuf::str_view sv() const  { return zbuf::str_view(reinterpret_cast<const char*>(this->ptr), this->len); }

		inline Span& remove_prefix(size_t n) { n = detail::min(n, this->len); this->ptr += n; this->len -= n; return *this; }
		inline Span& remove_suffix(size_t n) { n = detail::min(n, this->len); this->len -= n; return *this; }

		inline Span drop(size_t n) const { auto copy = *this; return copy.remove_prefix(n); }
		inline Span take(size_t n) const { auto copy = *this; return copy.remove_suffix(this->len > n ? this->len - n : 0); }

		inline Span take_last(size_t n) const { return (this->size() >= n ? Span(this->ptr + n, this->size() - n) : *this); }
		inline Span drop_last(size_t n) const { return (this->size() >= n ? Span(this->ptr, this->size() - n) : *this); }

		template <typename T>
		inline const T* as() const
		{
			return reinterpret_cast<const T*>(this->ptr);
		}

		inline uint8_t peek(size_t i = 0) const { return this->ptr[i]; }


	#if ZBUF_USE_STD
		static inline Span fromString(std::string_view sv)
		{
			return Span(reinterpret_cast<const uint8_t*>(sv.data()), sv.size());
		}
	#endif // ZBUF_USE_STD

	private:
		const uint8_t* ptr;
		size_t len;
	};



	// these depend on a definition of Span, so they need to be out of line.
	inline Span Buffer::span() const
	{
		return Span(this->ptr, this->len);
	}

	inline size_t Buffer::write(Span s)
	{
		return this->write(s.data(), s.size());
	}

	inline void Buffer::autoWrite(Span s)
	{
		return this->autoWrite(s.data(), s.size());
	}

	inline Span str_view::span() const
	{
		return Span(this->data(), this->size());
	}
}
