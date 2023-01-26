# -*- coding: utf-8 -*-
"""
Created on Fri Jun 21 22:13:41 2019

@author: revat
"""

from keras.datasets import cifar10

import cv2

import numpy as np

from sklearn.utils import shuffle
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix

from keras.models import Model, Sequential
from keras.layers import Input, Dense, Dropout, Flatten, Conv2D, MaxPooling2D, Activation
from keras.constraints import maxnorm
from keras.optimizers import SGD
from keras.preprocessing.image import ImageDataGenerator
from keras.layers import BatchNormalization
import os
import keras



(trainX, trainy), (testX, testy) = cifar10.load_data()

print('Train: X=%s, y=%s' % (trainX.shape, trainy.shape))
print('Test: X=%s, y=%s' % (testX.shape, testy.shape))


from matplotlib import pyplot
# plot first few images
for i in range(9):
    pyplot.subplot(330 + 1 + i)
    # plot raw pixel data
    pyplot.imshow(trainX[i])
# show the figure
pyplot.show()


# Convert class vectors to binary class matrices.
num_classes =10
trainy = keras.utils.to_categorical(trainy, num_classes)
testy = keras.utils.to_categorical(testy, num_classes)

##Preprocess data
trainX = trainX.astype('float32')
trainX/=255

testX = testX.astype('float32')
testX/=255


input_shape = trainX.shape[1:]
input_shape


###Data Augmentation

from keras.preprocessing.image import ImageDataGenerator
BATCH_SIZE = 64

# Create train generator.
train_datagen = ImageDataGenerator(rotation_range=10, 
                                   width_shift_range=0.1,
                                   height_shift_range=0.1, 
                                   horizontal_flip = 'true')

### model bi=uilding

##1st model with augmentation 

model = Sequential()

model.add(Conv2D(32, (3, 3), activation='relu', input_shape=(32,32,3)))
model.add(Activation('relu'))
model.add(BatchNormalization())
model.add(Conv2D(32, (3, 3)))
model.add(Activation('relu'))
model.add(BatchNormalization())
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.25))

model.add(Conv2D(64, (3, 3), padding='same'))
model.add(Activation('relu'))
model.add(BatchNormalization())
model.add(Conv2D(64, (3, 3)))
model.add(Activation('relu'))
model.add(BatchNormalization())
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.25))


model.add(Conv2D(128, (3, 3), padding='same'))
model.add(Activation('relu'))
model.add(BatchNormalization())
model.add(Conv2D(128, (3, 3)))
model.add(Activation('relu'))
model.add(BatchNormalization())
model.add(MaxPooling2D(pool_size=(2, 2)))
model.add(Dropout(0.25))

model.add(Flatten())
model.add(Dense(512))
model.add(Activation('relu'))
model.add(Dropout(0.5))
model.add(Dense(num_classes))
model.add(Activation('softmax'))

# initiate RMSprop optimizer
opt = keras.optimizers.rmsprop(lr=0.0001, decay=1e-6)

# Let's train the model using RMSprop
model.compile(loss='categorical_crossentropy',
              optimizer=opt,
              metrics=['accuracy'])


model.summary()

train_datagen.fit(trainX)


hist = model.fit_generator(train_datagen.flow(trainX, trainy,
                                     batch_size=64), epochs=110, validation_data=(testX, testy), workers = 4)



score = model.evaluate(testX, testy, batch_size=64)
print('Test Loss:', score[0])
print('Test Accuracy:', score[1])

ypred = model.predict(testX)

model.save("D:\\cifar-10_imageclassification\\cifar_model.h5")
print(model.predict_classes(testX))

print(ypred)

ypred = np.argmax(ypred, axis=1)
print(ypred)
print(confusion_matrix(np.argmax(testy,axis=1), ypred))

