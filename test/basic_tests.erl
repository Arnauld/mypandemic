%% run this command to compile all needed modules and to run tests
%% >  c("test/basic_tests"), c("src/basic"), eunit:test(basic_tests, [verbose]).
-module(basic_tests).

%% Include this lib so you can do tests
-include_lib("eunit/include/eunit.hrl").

a_test_function_ends_with_test() ->
  ActualValue = correct_this_value,
  ExpectedValue = true,
  ?assertEqual(ExpectedValue, ActualValue).

%% add 't' iteratively at the end of the functions so they are detected as tests
divide_should_return_the_division_of_two_numbers__tes() ->
  Dividend = 5,
  Divisor = 2,
  Quotient = 2.5,
  ?assertEqual(Quotient, basic:divide(Dividend, Divisor)).

%% 3
divide_should_return_undefined_when_divisor_is_0__tes() ->
  Dividend = 5,
  Divisor = 0,
  Quotient = undefined,
  ?assertEqual(Quotient, basic:divide(Dividend, Divisor)).

%% 4
sum_should_return_the_sum_of_numbers__tes() ->
  A = 3,
  B = 4,
  ?assertEqual(7, basic:sum(A, B)).

%% 5
sum_should_return_the_sum_of_two_3D_vectors__tes() ->
  A = {5, 3, 6},
  B = {1, 2, 3},
  ?assertEqual({6, 5, 9}, basic:sum(A, B)).

%% 6
sum_should_return_the_sum_all_elements_of_a_list__tes() ->
  L = [1, 2, 3, 4, 5],
  ?assertEqual(15, basic:sum(L)).

%% 7
%% apply with '+' or '/' for case of
apply_should_return_the_sum_of_2_elements_when_operator_is_plus__tes() ->
  Operator = '+',
  ?assertEqual(8, basic:apply(4, Operator, 4)).

%% 8
apply_should_return_the_division_of_2_elements_when_operator_is_slash__tes() ->
  Operator = '/',
  ?assertEqual(1.0, basic:apply(4, Operator, 4)).

maps_example_test() ->
  EmptyMap = #{},
  UpdatedMap1 = EmptyMap#{"Robert Virding" => '@rvirding'},
  UpdatedMap2 = UpdatedMap1#{"Joe Armstrong" => '@joeerl'},
  UpdatedMap3 = UpdatedMap2#{"Arnauld Loyer" => '@aloyer', "Yvan Vu" => '@JeSuisSocial'},
  SearchValue = maps:get("Mike Williams", UpdatedMap3, cannot_found_him_on_twitter),
  ?assertEqual(cannot_found_him_on_twitter, SearchValue),
  ?assertEqual('@JeSuisSocial', maps:get("Yvan Vu", UpdatedMap3)).