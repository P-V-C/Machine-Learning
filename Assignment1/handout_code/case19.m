%% Read Image
RGB = im2double (imread ('kande1.JPG'));

%% Define training and test regions
tll = [328, 150]; % training: lower left
tur = [264, 330]; % training: upper right

%% Show image and regions
imshow (RGB);
hold on
  plot ([tur(2), tll(2), tll(2), tur(2), tur(2)], ...
        [tur(1), tur(1), tll(1), tll(1), tur(1)], ...
        'k-', 'linewidth', 2);
  text (tll(2), tur(1)-15, 'Training');

hold off

%% Extract regions
training_data = RGB (tur(1):tll(1), tll(2):tur(2), :);
training_data = reshape (training_data, size (training_data, 1) * size (training_data, 2), 3);


