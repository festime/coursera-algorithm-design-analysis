require 'minitest/autorun'
require 'minitest/pride'
require_relative './merge_sort'

class TestMergeSort < Minitest::Test
  def test_basic_correctness
    a = (1..1_000_000).to_a.shuffle

    assert_equal(MergeSort.sort(a), a.sort)
  end

  def test_even_number_of_elements
    a = [1, 2, 3, 4, 5, 6, 7].shuffle

    assert_equal(MergeSort.sort(a), a.sort)
  end

  def test_odd_number_of_elements
    a = [1, 2, 3, 4, 5, 6].shuffle

    assert_equal(MergeSort.sort(a), a.sort)
  end

  def test_two_elements
    a = [1, 0]

    assert_equal(MergeSort.sort(a), a.sort)
  end

  def test_one_element
    a = [0]

    assert_equal(MergeSort.sort(a), a.sort)
  end

  def test_no_element
    a = []

    assert_equal(MergeSort.sort(a), a.sort)
  end
end
