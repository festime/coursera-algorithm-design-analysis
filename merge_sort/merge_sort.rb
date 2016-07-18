module MergeSort
  def self.sort(array)
    if (array.length <= 1)
      array

    else
      middle_index = array.length / 2

      front_half_array = sort(array[0...middle_index])
      back_half_array = sort(array[middle_index..-1])
      merge(front_half_array, back_half_array)
    end
  end

  private

  def self.merge(front_half_array, back_half_array)
    result = []
    
    while !front_half_array.empty? || !back_half_array.empty?
      if front_half_array.empty?
        result << back_half_array.shift

      elsif back_half_array.empty?
        result << front_half_array.shift

      elsif front_half_array.first < back_half_array.first
        result << front_half_array.shift
      else
        result << back_half_array.shift
      end
    end

    result
  end
end
