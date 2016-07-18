class InversionCount
  attr_reader :inversion_count

  def initialize
    @inversion_count = 0
  end

  def execute(array)
    if (array.length <= 1)
      array

    else
      middle_index = array.length / 2

      sorted_left_half_array = execute(array[0...middle_index])
      sorted_right_half_array = execute(array[middle_index..-1])

      merge_and_count(sorted_left_half_array, sorted_right_half_array)
    end
  end

  private

  def merge_and_count(left_array, right_array)
    result = Array.new(left_array.length + right_array.length)
    left_index = 0
    right_index = 0
    result_index = 0

    while left_index < left_array.length || right_index < right_array.length do
      left_value = left_array[left_index]
      right_value = right_array[right_index]

      if left_value.nil? && !right_value.nil?
        result[result_index] = right_value
        right_index += 1

      elsif right_value.nil? && !left_value.nil?
        result[result_index] = left_value
        left_index += 1

      elsif (left_value <= right_value)
        result[result_index] = left_value
        left_index += 1

      elsif (left_value > right_value)
        result[result_index] = right_value
        right_index += 1
        @inversion_count += (left_array.length - left_index)
      end

      result_index += 1
    end

    result
  end
end
