#include <iostream>
#include <vector>
#include <random>
#include "types.hh"

using namespace mighty;

template <typename URNG>
auto make_testcase (int n, URNG&& gen)
{
	std::uniform_int_distribution <> dist1 {0, 99};
	std::uniform_int_distribution <> dist2 {0, n - 1};
	std::vector <any_language> v (n);

	for (auto& place : v)
	{
		bool use_alt = dist1 (gen) < 50;

		const language* left;
		if (dist1 (gen) < 90)
			left = &v[dist2 (gen)].L;
		else if (dist1 (gen) < 50)
			left = make_empty ();
		else
			left = make_null ();

		const language* right;
		if (dist1 (gen) < 90)
			right = &v[dist2 (gen)].L;
		else if (dist1 (gen) < 50)
			right = make_empty ();
		else
			right = make_null ();

		if (use_alt)
			make_alternate (&place, left, right);
		else
			make_catenate (&place, left, right);
	}

	return v;
}

auto pos (const language* L, const std::vector <any_language>& tc)
{
	return reinterpret_cast <const any_language*> (L) - &tc[0];
};

void print_pos_or_string (const language* L, const std::vector <any_language>& tc)
{
	switch (type (L))
	{
	case language_type::root:
		std::cout << ":root";
		break;
	case language_type::empty:
		std::cout << "{}";
		break;
	case language_type::null:
		std::cout << "{epsilon}";
		break;
	case language_type::terminal:
		std::cout << "{c}";
		break;
	case language_type::repeat:
		print_pos_or_string (downcast <language_type::repeat> (L)->sublanguage, tc);
		std::cout << "*";
		break;
	default:
		std::cout << pos (L, tc);
	}
}

int main ()
{
	auto gen = std::mt19937 {};

	for (int i = 1; i <= 5; ++i)
	{
		std::cout << "Test " << i << std::endl;

		auto tc = make_testcase (20, gen);
		for (const auto& place : tc)
		{
			auto L = &place.L;

			std::cout << pos (L, tc) << " = ";
			if (type (L) == language_type::alternate)
			{
				std::cout << "(OR ";
				print_pos_or_string (downcast <language_type::alternate> (L)->left, tc);
				std::cout << " ";
				print_pos_or_string (downcast <language_type::alternate> (L)->right, tc);
			}
			else
			{
				std::cout << "(AND ";
				print_pos_or_string (downcast <language_type::catenate> (L)->left, tc);
				std::cout << " ";
				print_pos_or_string (downcast <language_type::catenate> (L)->right, tc);
			}
			std::cout << ")\n";
		}
	}
}
