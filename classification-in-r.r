{
 "cells": [
  {
   "cell_type": "markdown",
   "id": "c75dc3a1",
   "metadata": {
    "papermill": {
     "duration": 0.016953,
     "end_time": "2022-05-23T22:20:13.520244",
     "exception": false,
     "start_time": "2022-05-23T22:20:13.503291",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    ">> In this notebook we will be using the different Machine_Learning algorithms in R. Such as-\n",
    "1. Logistic Regrssion\n",
    "2. Classification Trees\n",
    "3. Naive Bayes classifier\n",
    "4. K-nearest neighbours\n",
    "5. Support Vector Machines\n",
    "6. Neural Networks\n",
    "7. Ensemble\n",
    "8. Random Forests\n",
    "9. Gradient Boosting Machines\n"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "fe777de7",
   "metadata": {
    "papermill": {
     "duration": 0.013547,
     "end_time": "2022-05-23T22:20:13.547476",
     "exception": false,
     "start_time": "2022-05-23T22:20:13.533929",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "> > Logistic Regression"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 1,
   "id": "a29844ae",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:13.578988Z",
     "iopub.status.busy": "2022-05-23T22:20:13.576094Z",
     "iopub.status.idle": "2022-05-23T22:20:13.750420Z",
     "shell.execute_reply": "2022-05-23T22:20:13.747468Z"
    },
    "papermill": {
     "duration": 0.192978,
     "end_time": "2022-05-23T22:20:13.753451",
     "exception": false,
     "start_time": "2022-05-23T22:20:13.560473",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table class=\"dataframe\">\n",
       "<caption>A data.frame: 6 × 6</caption>\n",
       "<thead>\n",
       "\t<tr><th></th><th scope=col>Id</th><th scope=col>SepalLengthCm</th><th scope=col>SepalWidthCm</th><th scope=col>PetalLengthCm</th><th scope=col>PetalWidthCm</th><th scope=col>Species</th></tr>\n",
       "\t<tr><th></th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th></tr>\n",
       "</thead>\n",
       "<tbody>\n",
       "\t<tr><th scope=row>1</th><td>1</td><td>5.1</td><td>3.5</td><td>1.4</td><td>0.2</td><td>Iris-setosa</td></tr>\n",
       "\t<tr><th scope=row>2</th><td>2</td><td>4.9</td><td>3.0</td><td>1.4</td><td>0.2</td><td>Iris-setosa</td></tr>\n",
       "\t<tr><th scope=row>3</th><td>3</td><td>4.7</td><td>3.2</td><td>1.3</td><td>0.2</td><td>Iris-setosa</td></tr>\n",
       "\t<tr><th scope=row>4</th><td>4</td><td>4.6</td><td>3.1</td><td>1.5</td><td>0.2</td><td>Iris-setosa</td></tr>\n",
       "\t<tr><th scope=row>5</th><td>5</td><td>5.0</td><td>3.6</td><td>1.4</td><td>0.2</td><td>Iris-setosa</td></tr>\n",
       "\t<tr><th scope=row>6</th><td>6</td><td>5.4</td><td>3.9</td><td>1.7</td><td>0.4</td><td>Iris-setosa</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A data.frame: 6 × 6\n",
       "\\begin{tabular}{r|llllll}\n",
       "  & Id & SepalLengthCm & SepalWidthCm & PetalLengthCm & PetalWidthCm & Species\\\\\n",
       "  & <int> & <dbl> & <dbl> & <dbl> & <dbl> & <chr>\\\\\n",
       "\\hline\n",
       "\t1 & 1 & 5.1 & 3.5 & 1.4 & 0.2 & Iris-setosa\\\\\n",
       "\t2 & 2 & 4.9 & 3.0 & 1.4 & 0.2 & Iris-setosa\\\\\n",
       "\t3 & 3 & 4.7 & 3.2 & 1.3 & 0.2 & Iris-setosa\\\\\n",
       "\t4 & 4 & 4.6 & 3.1 & 1.5 & 0.2 & Iris-setosa\\\\\n",
       "\t5 & 5 & 5.0 & 3.6 & 1.4 & 0.2 & Iris-setosa\\\\\n",
       "\t6 & 6 & 5.4 & 3.9 & 1.7 & 0.4 & Iris-setosa\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A data.frame: 6 × 6\n",
       "\n",
       "| <!--/--> | Id &lt;int&gt; | SepalLengthCm &lt;dbl&gt; | SepalWidthCm &lt;dbl&gt; | PetalLengthCm &lt;dbl&gt; | PetalWidthCm &lt;dbl&gt; | Species &lt;chr&gt; |\n",
       "|---|---|---|---|---|---|---|\n",
       "| 1 | 1 | 5.1 | 3.5 | 1.4 | 0.2 | Iris-setosa |\n",
       "| 2 | 2 | 4.9 | 3.0 | 1.4 | 0.2 | Iris-setosa |\n",
       "| 3 | 3 | 4.7 | 3.2 | 1.3 | 0.2 | Iris-setosa |\n",
       "| 4 | 4 | 4.6 | 3.1 | 1.5 | 0.2 | Iris-setosa |\n",
       "| 5 | 5 | 5.0 | 3.6 | 1.4 | 0.2 | Iris-setosa |\n",
       "| 6 | 6 | 5.4 | 3.9 | 1.7 | 0.4 | Iris-setosa |\n",
       "\n"
      ],
      "text/plain": [
       "  Id SepalLengthCm SepalWidthCm PetalLengthCm PetalWidthCm Species    \n",
       "1 1  5.1           3.5          1.4           0.2          Iris-setosa\n",
       "2 2  4.9           3.0          1.4           0.2          Iris-setosa\n",
       "3 3  4.7           3.2          1.3           0.2          Iris-setosa\n",
       "4 4  4.6           3.1          1.5           0.2          Iris-setosa\n",
       "5 5  5.0           3.6          1.4           0.2          Iris-setosa\n",
       "6 6  5.4           3.9          1.7           0.4          Iris-setosa"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#loading the dataset in R\n",
    "data1 <- read.csv(\"../input/iris/Iris.csv\")\n",
    "#viewing it\n",
    "head(data1)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 2,
   "id": "7320f491",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:13.823139Z",
     "iopub.status.busy": "2022-05-23T22:20:13.781412Z",
     "iopub.status.idle": "2022-05-23T22:20:13.850312Z",
     "shell.execute_reply": "2022-05-23T22:20:13.848021Z"
    },
    "papermill": {
     "duration": 0.085953,
     "end_time": "2022-05-23T22:20:13.853111",
     "exception": false,
     "start_time": "2022-05-23T22:20:13.767158",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table class=\"dataframe\">\n",
       "<caption>A data.frame: 6 × 6</caption>\n",
       "<thead>\n",
       "\t<tr><th></th><th scope=col>Id</th><th scope=col>SepalLengthCm</th><th scope=col>SepalWidthCm</th><th scope=col>PetalLengthCm</th><th scope=col>PetalWidthCm</th><th scope=col>Species</th></tr>\n",
       "\t<tr><th></th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th></tr>\n",
       "</thead>\n",
       "<tbody>\n",
       "\t<tr><th scope=row>145</th><td>145</td><td>6.7</td><td>3.3</td><td>5.7</td><td>2.5</td><td>Iris-virginica</td></tr>\n",
       "\t<tr><th scope=row>146</th><td>146</td><td>6.7</td><td>3.0</td><td>5.2</td><td>2.3</td><td>Iris-virginica</td></tr>\n",
       "\t<tr><th scope=row>147</th><td>147</td><td>6.3</td><td>2.5</td><td>5.0</td><td>1.9</td><td>Iris-virginica</td></tr>\n",
       "\t<tr><th scope=row>148</th><td>148</td><td>6.5</td><td>3.0</td><td>5.2</td><td>2.0</td><td>Iris-virginica</td></tr>\n",
       "\t<tr><th scope=row>149</th><td>149</td><td>6.2</td><td>3.4</td><td>5.4</td><td>2.3</td><td>Iris-virginica</td></tr>\n",
       "\t<tr><th scope=row>150</th><td>150</td><td>5.9</td><td>3.0</td><td>5.1</td><td>1.8</td><td>Iris-virginica</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A data.frame: 6 × 6\n",
       "\\begin{tabular}{r|llllll}\n",
       "  & Id & SepalLengthCm & SepalWidthCm & PetalLengthCm & PetalWidthCm & Species\\\\\n",
       "  & <int> & <dbl> & <dbl> & <dbl> & <dbl> & <chr>\\\\\n",
       "\\hline\n",
       "\t145 & 145 & 6.7 & 3.3 & 5.7 & 2.5 & Iris-virginica\\\\\n",
       "\t146 & 146 & 6.7 & 3.0 & 5.2 & 2.3 & Iris-virginica\\\\\n",
       "\t147 & 147 & 6.3 & 2.5 & 5.0 & 1.9 & Iris-virginica\\\\\n",
       "\t148 & 148 & 6.5 & 3.0 & 5.2 & 2.0 & Iris-virginica\\\\\n",
       "\t149 & 149 & 6.2 & 3.4 & 5.4 & 2.3 & Iris-virginica\\\\\n",
       "\t150 & 150 & 5.9 & 3.0 & 5.1 & 1.8 & Iris-virginica\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A data.frame: 6 × 6\n",
       "\n",
       "| <!--/--> | Id &lt;int&gt; | SepalLengthCm &lt;dbl&gt; | SepalWidthCm &lt;dbl&gt; | PetalLengthCm &lt;dbl&gt; | PetalWidthCm &lt;dbl&gt; | Species &lt;chr&gt; |\n",
       "|---|---|---|---|---|---|---|\n",
       "| 145 | 145 | 6.7 | 3.3 | 5.7 | 2.5 | Iris-virginica |\n",
       "| 146 | 146 | 6.7 | 3.0 | 5.2 | 2.3 | Iris-virginica |\n",
       "| 147 | 147 | 6.3 | 2.5 | 5.0 | 1.9 | Iris-virginica |\n",
       "| 148 | 148 | 6.5 | 3.0 | 5.2 | 2.0 | Iris-virginica |\n",
       "| 149 | 149 | 6.2 | 3.4 | 5.4 | 2.3 | Iris-virginica |\n",
       "| 150 | 150 | 5.9 | 3.0 | 5.1 | 1.8 | Iris-virginica |\n",
       "\n"
      ],
      "text/plain": [
       "    Id  SepalLengthCm SepalWidthCm PetalLengthCm PetalWidthCm Species       \n",
       "145 145 6.7           3.3          5.7           2.5          Iris-virginica\n",
       "146 146 6.7           3.0          5.2           2.3          Iris-virginica\n",
       "147 147 6.3           2.5          5.0           1.9          Iris-virginica\n",
       "148 148 6.5           3.0          5.2           2.0          Iris-virginica\n",
       "149 149 6.2           3.4          5.4           2.3          Iris-virginica\n",
       "150 150 5.9           3.0          5.1           1.8          Iris-virginica"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "tail(data1)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 3,
   "id": "06ff9283",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:13.882647Z",
     "iopub.status.busy": "2022-05-23T22:20:13.880909Z",
     "iopub.status.idle": "2022-05-23T22:20:13.900094Z",
     "shell.execute_reply": "2022-05-23T22:20:13.898150Z"
    },
    "papermill": {
     "duration": 0.036522,
     "end_time": "2022-05-23T22:20:13.902833",
     "exception": false,
     "start_time": "2022-05-23T22:20:13.866311",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "150"
      ],
      "text/latex": [
       "150"
      ],
      "text/markdown": [
       "150"
      ],
      "text/plain": [
       "[1] 150"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "nrow(data1)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 4,
   "id": "a108edc1",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:13.933161Z",
     "iopub.status.busy": "2022-05-23T22:20:13.931531Z",
     "iopub.status.idle": "2022-05-23T22:20:13.953163Z",
     "shell.execute_reply": "2022-05-23T22:20:13.951262Z"
    },
    "papermill": {
     "duration": 0.039513,
     "end_time": "2022-05-23T22:20:13.955845",
     "exception": false,
     "start_time": "2022-05-23T22:20:13.916332",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "6"
      ],
      "text/latex": [
       "6"
      ],
      "text/markdown": [
       "6"
      ],
      "text/plain": [
       "[1] 6"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "ncol(data1)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 5,
   "id": "945deae9",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:13.986153Z",
     "iopub.status.busy": "2022-05-23T22:20:13.984344Z",
     "iopub.status.idle": "2022-05-23T22:20:14.006955Z",
     "shell.execute_reply": "2022-05-23T22:20:14.005049Z"
    },
    "papermill": {
     "duration": 0.040293,
     "end_time": "2022-05-23T22:20:14.009739",
     "exception": false,
     "start_time": "2022-05-23T22:20:13.969446",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "       Id         SepalLengthCm    SepalWidthCm   PetalLengthCm  \n",
       " Min.   :  1.00   Min.   :4.300   Min.   :2.000   Min.   :1.000  \n",
       " 1st Qu.: 38.25   1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600  \n",
       " Median : 75.50   Median :5.800   Median :3.000   Median :4.350  \n",
       " Mean   : 75.50   Mean   :5.843   Mean   :3.054   Mean   :3.759  \n",
       " 3rd Qu.:112.75   3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100  \n",
       " Max.   :150.00   Max.   :7.900   Max.   :4.400   Max.   :6.900  \n",
       "  PetalWidthCm     Species         \n",
       " Min.   :0.100   Length:150        \n",
       " 1st Qu.:0.300   Class :character  \n",
       " Median :1.300   Mode  :character  \n",
       " Mean   :1.199                     \n",
       " 3rd Qu.:1.800                     \n",
       " Max.   :2.500                     "
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "summary(data1)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 6,
   "id": "ff2886c5",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:14.040062Z",
     "iopub.status.busy": "2022-05-23T22:20:14.038329Z",
     "iopub.status.idle": "2022-05-23T22:20:14.056732Z",
     "shell.execute_reply": "2022-05-23T22:20:14.054865Z"
    },
    "papermill": {
     "duration": 0.036239,
     "end_time": "2022-05-23T22:20:14.059589",
     "exception": false,
     "start_time": "2022-05-23T22:20:14.023350",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "0"
      ],
      "text/latex": [
       "0"
      ],
      "text/markdown": [
       "0"
      ],
      "text/plain": [
       "[1] 0"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "sum(is.na(data1)) #as we can see there are no na values available in our dataset"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 7,
   "id": "f9d7719a",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:14.090592Z",
     "iopub.status.busy": "2022-05-23T22:20:14.089103Z",
     "iopub.status.idle": "2022-05-23T22:20:14.118468Z",
     "shell.execute_reply": "2022-05-23T22:20:14.116576Z"
    },
    "papermill": {
     "duration": 0.048073,
     "end_time": "2022-05-23T22:20:14.121511",
     "exception": false,
     "start_time": "2022-05-23T22:20:14.073438",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "#we will going to imploy the simple test train methods in r\n",
    "n <- nrow(data1)#number of observation\n",
    "ntrain <- round(n*0.6) #60% for training set\n",
    "set.seed(333)#setting the seed\n",
    "#creating an index\n",
    "tindex <- sample(n,ntrain)\n",
    "train_iris <- data1[tindex,] #training one\n",
    "test_iris <- data1[-tindex,] #testing one\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 8,
   "id": "fecb712e",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:14.152094Z",
     "iopub.status.busy": "2022-05-23T22:20:14.150563Z",
     "iopub.status.idle": "2022-05-23T22:20:14.168543Z",
     "shell.execute_reply": "2022-05-23T22:20:14.166579Z"
    },
    "papermill": {
     "duration": 0.03609,
     "end_time": "2022-05-23T22:20:14.171420",
     "exception": false,
     "start_time": "2022-05-23T22:20:14.135330",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "90"
      ],
      "text/latex": [
       "90"
      ],
      "text/markdown": [
       "90"
      ],
      "text/plain": [
       "[1] 90"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "nrow(train_iris)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 9,
   "id": "88b7824a",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:14.201281Z",
     "iopub.status.busy": "2022-05-23T22:20:14.199846Z",
     "iopub.status.idle": "2022-05-23T22:20:14.217239Z",
     "shell.execute_reply": "2022-05-23T22:20:14.215363Z"
    },
    "papermill": {
     "duration": 0.034968,
     "end_time": "2022-05-23T22:20:14.219858",
     "exception": false,
     "start_time": "2022-05-23T22:20:14.184890",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "60"
      ],
      "text/latex": [
       "60"
      ],
      "text/markdown": [
       "60"
      ],
      "text/plain": [
       "[1] 60"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "nrow(test_iris)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 10,
   "id": "7e454ce6",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:14.251716Z",
     "iopub.status.busy": "2022-05-23T22:20:14.250066Z",
     "iopub.status.idle": "2022-05-23T22:20:14.304906Z",
     "shell.execute_reply": "2022-05-23T22:20:14.302668Z"
    },
    "papermill": {
     "duration": 0.072988,
     "end_time": "2022-05-23T22:20:14.307741",
     "exception": false,
     "start_time": "2022-05-23T22:20:14.234753",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table class=\"dataframe\">\n",
       "<caption>A data.frame: 6 × 1</caption>\n",
       "<thead>\n",
       "\t<tr><th></th><th scope=col>isVersicolor</th></tr>\n",
       "\t<tr><th></th><th scope=col>&lt;lgl&gt;</th></tr>\n",
       "</thead>\n",
       "<tbody>\n",
       "\t<tr><th scope=row>1</th><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>2</th><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>3</th><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>4</th><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>5</th><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>6</th><td>FALSE</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A data.frame: 6 × 1\n",
       "\\begin{tabular}{r|l}\n",
       "  & isVersicolor\\\\\n",
       "  & <lgl>\\\\\n",
       "\\hline\n",
       "\t1 & FALSE\\\\\n",
       "\t2 &  TRUE\\\\\n",
       "\t3 &  TRUE\\\\\n",
       "\t4 & FALSE\\\\\n",
       "\t5 &  TRUE\\\\\n",
       "\t6 & FALSE\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A data.frame: 6 × 1\n",
       "\n",
       "| <!--/--> | isVersicolor &lt;lgl&gt; |\n",
       "|---|---|\n",
       "| 1 | FALSE |\n",
       "| 2 |  TRUE |\n",
       "| 3 |  TRUE |\n",
       "| 4 | FALSE |\n",
       "| 5 |  TRUE |\n",
       "| 6 | FALSE |\n",
       "\n"
      ],
      "text/plain": [
       "  isVersicolor\n",
       "1 FALSE       \n",
       "2  TRUE       \n",
       "3  TRUE       \n",
       "4 FALSE       \n",
       "5  TRUE       \n",
       "6 FALSE       "
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#so in order to classify into the different label class, first we need to create the different columns of that class\n",
    "newcol <- data.frame(isVersicolor=(train_iris$Species ==\"Iris-versicolor\"))\n",
    "#let's see this new dataframe\n",
    "head(newcol)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 11,
   "id": "da7a55b6",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:14.338854Z",
     "iopub.status.busy": "2022-05-23T22:20:14.337316Z",
     "iopub.status.idle": "2022-05-23T22:20:14.356026Z",
     "shell.execute_reply": "2022-05-23T22:20:14.353801Z"
    },
    "papermill": {
     "duration": 0.037146,
     "end_time": "2022-05-23T22:20:14.358947",
     "exception": false,
     "start_time": "2022-05-23T22:20:14.321801",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "90"
      ],
      "text/latex": [
       "90"
      ],
      "text/markdown": [
       "90"
      ],
      "text/plain": [
       "[1] 90"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "nrow(newcol)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 12,
   "id": "921b11e9",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:14.391822Z",
     "iopub.status.busy": "2022-05-23T22:20:14.390248Z",
     "iopub.status.idle": "2022-05-23T22:20:14.426860Z",
     "shell.execute_reply": "2022-05-23T22:20:14.424774Z"
    },
    "papermill": {
     "duration": 0.05587,
     "end_time": "2022-05-23T22:20:14.429720",
     "exception": false,
     "start_time": "2022-05-23T22:20:14.373850",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table class=\"dataframe\">\n",
       "<caption>A data.frame: 6 × 7</caption>\n",
       "<thead>\n",
       "\t<tr><th></th><th scope=col>Id</th><th scope=col>SepalLengthCm</th><th scope=col>SepalWidthCm</th><th scope=col>PetalLengthCm</th><th scope=col>PetalWidthCm</th><th scope=col>Species</th><th scope=col>isVersicolor</th></tr>\n",
       "\t<tr><th></th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;lgl&gt;</th></tr>\n",
       "</thead>\n",
       "<tbody>\n",
       "\t<tr><th scope=row>142</th><td>142</td><td>6.9</td><td>3.1</td><td>5.1</td><td>2.3</td><td>Iris-virginica </td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>55</th><td> 55</td><td>6.5</td><td>2.8</td><td>4.6</td><td>1.5</td><td>Iris-versicolor</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>66</th><td> 66</td><td>6.7</td><td>3.1</td><td>4.4</td><td>1.4</td><td>Iris-versicolor</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>39</th><td> 39</td><td>4.4</td><td>3.0</td><td>1.3</td><td>0.2</td><td>Iris-setosa    </td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>60</th><td> 60</td><td>5.2</td><td>2.7</td><td>3.9</td><td>1.4</td><td>Iris-versicolor</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>2</th><td>  2</td><td>4.9</td><td>3.0</td><td>1.4</td><td>0.2</td><td>Iris-setosa    </td><td>FALSE</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A data.frame: 6 × 7\n",
       "\\begin{tabular}{r|lllllll}\n",
       "  & Id & SepalLengthCm & SepalWidthCm & PetalLengthCm & PetalWidthCm & Species & isVersicolor\\\\\n",
       "  & <int> & <dbl> & <dbl> & <dbl> & <dbl> & <chr> & <lgl>\\\\\n",
       "\\hline\n",
       "\t142 & 142 & 6.9 & 3.1 & 5.1 & 2.3 & Iris-virginica  & FALSE\\\\\n",
       "\t55 &  55 & 6.5 & 2.8 & 4.6 & 1.5 & Iris-versicolor &  TRUE\\\\\n",
       "\t66 &  66 & 6.7 & 3.1 & 4.4 & 1.4 & Iris-versicolor &  TRUE\\\\\n",
       "\t39 &  39 & 4.4 & 3.0 & 1.3 & 0.2 & Iris-setosa     & FALSE\\\\\n",
       "\t60 &  60 & 5.2 & 2.7 & 3.9 & 1.4 & Iris-versicolor &  TRUE\\\\\n",
       "\t2 &   2 & 4.9 & 3.0 & 1.4 & 0.2 & Iris-setosa     & FALSE\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A data.frame: 6 × 7\n",
       "\n",
       "| <!--/--> | Id &lt;int&gt; | SepalLengthCm &lt;dbl&gt; | SepalWidthCm &lt;dbl&gt; | PetalLengthCm &lt;dbl&gt; | PetalWidthCm &lt;dbl&gt; | Species &lt;chr&gt; | isVersicolor &lt;lgl&gt; |\n",
       "|---|---|---|---|---|---|---|---|\n",
       "| 142 | 142 | 6.9 | 3.1 | 5.1 | 2.3 | Iris-virginica  | FALSE |\n",
       "| 55 |  55 | 6.5 | 2.8 | 4.6 | 1.5 | Iris-versicolor |  TRUE |\n",
       "| 66 |  66 | 6.7 | 3.1 | 4.4 | 1.4 | Iris-versicolor |  TRUE |\n",
       "| 39 |  39 | 4.4 | 3.0 | 1.3 | 0.2 | Iris-setosa     | FALSE |\n",
       "| 60 |  60 | 5.2 | 2.7 | 3.9 | 1.4 | Iris-versicolor |  TRUE |\n",
       "| 2 |   2 | 4.9 | 3.0 | 1.4 | 0.2 | Iris-setosa     | FALSE |\n",
       "\n"
      ],
      "text/plain": [
       "    Id  SepalLengthCm SepalWidthCm PetalLengthCm PetalWidthCm Species        \n",
       "142 142 6.9           3.1          5.1           2.3          Iris-virginica \n",
       "55   55 6.5           2.8          4.6           1.5          Iris-versicolor\n",
       "66   66 6.7           3.1          4.4           1.4          Iris-versicolor\n",
       "39   39 4.4           3.0          1.3           0.2          Iris-setosa    \n",
       "60   60 5.2           2.7          3.9           1.4          Iris-versicolor\n",
       "2     2 4.9           3.0          1.4           0.2          Iris-setosa    \n",
       "    isVersicolor\n",
       "142 FALSE       \n",
       "55   TRUE       \n",
       "66   TRUE       \n",
       "39  FALSE       \n",
       "60   TRUE       \n",
       "2   FALSE       "
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#now we will join it to our test dataframe\n",
    "train_iris <- cbind(train_iris,newcol)\n",
    "head(train_iris)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 13,
   "id": "854cf1f9",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:14.462475Z",
     "iopub.status.busy": "2022-05-23T22:20:14.460775Z",
     "iopub.status.idle": "2022-05-23T22:20:14.503808Z",
     "shell.execute_reply": "2022-05-23T22:20:14.501901Z"
    },
    "papermill": {
     "duration": 0.062313,
     "end_time": "2022-05-23T22:20:14.506817",
     "exception": false,
     "start_time": "2022-05-23T22:20:14.444504",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "\n",
       "Call:\n",
       "glm(formula = isVersicolor ~ SepalWidthCm, family = binomial, \n",
       "    data = train_iris)\n",
       "\n",
       "Deviance Residuals: \n",
       "    Min       1Q   Median       3Q      Max  \n",
       "-1.7472  -0.8936  -0.3978   0.7991   2.0230  \n",
       "\n",
       "Coefficients:\n",
       "             Estimate Std. Error z value Pr(>|z|)    \n",
       "(Intercept)   11.2480     2.9317   3.837 0.000125 ***\n",
       "SepalWidthCm  -3.9866     0.9931  -4.014 5.96e-05 ***\n",
       "---\n",
       "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n",
       "\n",
       "(Dispersion parameter for binomial family taken to be 1)\n",
       "\n",
       "    Null deviance: 115.909  on 89  degrees of freedom\n",
       "Residual deviance:  86.715  on 88  degrees of freedom\n",
       "AIC: 90.715\n",
       "\n",
       "Number of Fisher Scoring iterations: 5\n"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#now we will run the logistic regression model\n",
    "glm1 <- glm(isVersicolor ~ SepalWidthCm,data= train_iris,family=binomial)\n",
    "summary(glm1)"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "48f92ed9",
   "metadata": {
    "papermill": {
     "duration": 0.014754,
     "end_time": "2022-05-23T22:20:14.536562",
     "exception": false,
     "start_time": "2022-05-23T22:20:14.521808",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "> > Plotting the results"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 14,
   "id": "d14f1fc2",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:14.569672Z",
     "iopub.status.busy": "2022-05-23T22:20:14.567979Z",
     "iopub.status.idle": "2022-05-23T22:20:14.819975Z",
     "shell.execute_reply": "2022-05-23T22:20:14.816843Z"
    },
    "papermill": {
     "duration": 0.271931,
     "end_time": "2022-05-23T22:20:14.823354",
     "exception": false,
     "start_time": "2022-05-23T22:20:14.551423",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "image/png": "iVBORw0KGgoAAAANSUhEUgAAA0gAAANICAIAAAByhViMAAAABmJLR0QA/wD/AP+gvaeTAAAg\nAElEQVR4nOzdZ0AU18LG8bOwgHQULCgaRYliV1TsvWFEjcZesccGCgpB7GKvWLFgwRaNvUSN\nNYmK3dgVG3aCiKhIXfb9sPd6fRVxgV2GXf6/TzgzHp911+yTmTlnZEqlUgAAAED3GUgdAAAA\nAJpBsQMAANATFDsAAAA9QbEDAADQExQ7AAAAPUGxAwAA0BMUOwAAAD1BsQMAANATFDsAAAA9\nQbEDAADQExQ7AAAAPUGxAwAA0BMUOwAAAD1BsQMAANATFDsAAAA9QbEDAADQExQ7AAAAPUGx\nAwAA0BMUOwAAAD1BsQMAANATFDsAAAA9QbEDAADQExQ7AAAAPUGxAwAA0BMUOwAAAD1BsQMA\nANATFDsAAAA9QbEDAADQExQ7AAAAPUGxAwAA0BMUOwAAAD1BsQMAANATFDsAAAA9QbEDAADQ\nExQ7AAAAPUGxAwAA0BMUOwAAAD1BsQMAANATFDsAAAA9QbEDAADQExQ7AAAAPUGxAwAA0BMU\nOwAAAD1BsQMAANATFDsAAAA9QbEDAADQExQ7AAAAPUGxAwAA0BMUOwAAAD1BsQMAANATFDsA\nAAA9QbEDAADQExQ7AAAAPUGxAwAA0BMUOwAAAD1BsQMAANATFDsAAAA9QbEDAADQExQ7AAAA\nPUGxAwAA0BMUOwAAAD1BsQMAANATFDsAAAA9QbEDAADQExQ7AAAAPUGxAwAA0BMUOwAAAD1B\nsQMAANATFDsAAAA9QbEDAADQExQ7AAAAPUGxAwAA0BMUOwAAAD1BsQMAANATFDsAAAA9QbED\nAADQExQ7AAAAPUGxAwAA0BMUOwAAAD1BsQMAANATFDsAAAA9QbEDAADQExQ7AAAAPUGxAwAA\n0BMUOwAAAD1BsQMAANATFDsAAAA9QbEDAADQExQ7AAAAPUGxAwAA0BMUOwAAAD1BsQMAANAT\nFDsAAAA9QbEDAADQExQ7AAAAPUGxAwAA0BNyqQPogNjY2HXr1sXHx0sdBAAA5Aimpqa9e/e2\ntraWOsjnKHbftnHjRk9PT6lTAACAHEQulw8ZMkTqFJ+j2H1bcnKyEGL16tWVKlWSOgsAAJDY\nP//8069fP1U9yGkoduoqXbq0i4uL1CkAAIDEEhISpI7wVUyeAAAA0BMUOwAAAD1BsQMAANAT\nFDsAAAA9QbEDAADQExQ7AAAAPaGry53EvHh450545Ou3cR8S5HnMrW0LOZVxdrS3kToXAACA\nZHSs2CkVsVvnTwpaven07cgv9xYqU7Nbf89xnp1t5LLszwYAACAtXSp2iqRnHtUrhV6NNjTK\n59q4TUXnkvZ2NiYm8pTExDevXkaE3zj919l5Pl3Xb9r3z5n1hY25ygwAAHIXXSp2Z7xbhl6N\nrjts4eYZQxzM00iemhS9eebQnhM2NRve/0Zww2wPCAAAICVdOq3lHxpuYT/4r0Uj0mx1QggD\nY9vu47Yscy14f0tANmcDAACQnC4Vu2txyRbF3L95mEv9AskfbmRDHgAAgBxFl4pdW1vTmNsz\nXialpndQanzI1kd58rbIrlAAAAA5hS4Vu7EzWyTG/lW+ZqcNhy7GKZSf71Ym3vxrZ/9mzsse\nvW04YYIUAQEAAKSkS5MnnHpvW3m++aClO3q23G5obO3oVLJwfhsTEyNFUmLsqxcPwu+/TkiR\nyWSNhizZM9RZ6rBp+PDhw5EjR+7cuSOEKF26dNOmTc3MzKQO9f+EhITs378/Pj6+cOHCAwYM\ncHV1zcpor169WrBgwZUrV4QQlStX9vLysrOzy8qAT548OXr0aGRkpKmpqauraxbjCU2/Xo2/\nv2fPnl25cuXz589NTU1/+OGHvn37ZmU0IUTr1q3//vvvlJQUS0tLb29vHx+frIz25MkTb2/v\n69evCyHKly8/d+7cokWLZmXAvn37bty4MSUlRSaTlShRIjw8PCujCSECAgL27duXkJBQsGBB\nb2/vNm3aZHFAzdL45xkAhBBCqWuehu38ZVCnSt8XMzH432J1MgOTok6VOg8cs+vcU43/iQsW\nLBBC/P3331kZZPny5ba2tp/+zdva2gYHB2sqZBZt2rTJ1NT0s8+Gk5PT06eZ/Pvs3r27TPb/\nVhOUyWQ9evTI3GixsbEeHh6GhoafDlilSpXLly9nbkCNv17Nvr9Pnz51cnL6LJ6pqemWLVsy\nN2Dnzp2//LdvYGBw4sSJzA1Yo0aNLwd0dXXN3Gjr16//cjQhRKVKlTI34LRp0wwMPr8cYWVl\ndevWrcwNqFka/zwDyGZ///23EGLBggVSB0mD7hW7j1KTP7yOinzy+ElkVPSH5FTt/UFZL3ZT\np04VQhQvXnzhwoVnz549e/bswoULv/vuOyFEYGCgBqNmTmhoqKqEValSZd68eRs3bvT29lbV\nFEtLy5iYmIwO2LRpUyGEXC5v3759SEhISEhI+/bt5XK5EKJZs2YZHe3Dhw81a9YUQjRv3vy3\n3367ePHisWPHvLy8TExMLC0tL168mNEBNf56Nfv+RkdHW1hYqKqhj4/Pxo0b582bV6VKFSGE\nTCbbuHFjRgfs0KGDqjrI5fI6dep06tSpZMmSH/vE3bt3Mzqg6rfLZLJKlSqNGzdu3LhxlSpV\nUo1WqlSpjI52/Pjxj2HMzc3Lly9vb2//cUv58uUzOuDkyZNVv7dAgQJDhgwJDAx0d3c3MjIS\nQhgaGr58+TKjA2qWxj/PALIfxU63ZbHYXb9+XS6Xu7i4vHnz5tPtb968qVq1qlwuv379uiZi\nZlJycrLq3NXy5cs/26X6+mnatGmGBty1a5cQwszM7PHjx59uf/z4serS5K5duzI04JQpU4QQ\nfn5+n20/deqUqalppUqVFAqF+qNp/PVq/P1t0qSJEKJmzZqfbV++fLkQwtTUNDk5Wf3R4uLi\nVC2nefPmn+0yNjZWDZiheEFBQaqzfTdu3Ph0+40bN1QnyYKCgjI04MdTa59tL168uGp7hqp2\nYmKiasDRo0d/tqtw4cJCiJIlS2YonsZp9vMMQBIUO92WxWI3atQoIcTZs2e/3BUWFiaE8Pb2\nzlrALFm5cqUQonr16l/uio+PNzIyksvlGfqmcXFxEUKEhIR8uWv16tVCiGrVqqk/WmpqarFi\nxRwdHdNsM6q7xM6cOaP+gBp/vZp9fxUKhVwuNzIyio+P/3Jv9erVhRArV65Uf0A3NzchhJGR\n0Ze7oqKiVM0pKSlJ/QELFiwohBg3btyXu8aOHSuEKFiwoPqjxcfHqzI8fPjwy72qXY6OjuoP\n+MsvvwghChUq9OWud+/eqc7UpqSkqD+gZmn88wxAEjm52OnSrFh1JL09ZW9v/+mlnPQpFIq9\ne/duS9fly5eFEMnJyZmLdPHixfz583/tniQ7O7sLFy5kbmSNOHz4sBCiW7duX+7KkyePk5NT\nSkrK3bt31R/w/v37BgYGHh4eX+7q27evgYHBvXv31B8tKirq8ePHLVq0UF3J/UyrVq2EEBcv\nXlR/QI2/Xs2+v7dv305JSfn+++/z5Mnz5d6uXbsKIf744w/1Bzxz5owqyZe77OzsVLd5/fbb\nb+oPGB0dLYT4eLnzU6pL0qoD1BQYGKj64eP5uU+p/iFHRESoP+DBgweFEB+vPn/KwsLCxsZG\nCHH27Fn1B9QsjX+eAeAzujQrVh1KZdLLly/VP/748eNqzpXbtGlTw4YNMxHp7du31tbWX9tr\nY2Pz7t27TAyrKbGxseK/36BfUiV/8eJFmTJl1BwwMTHxs7vCP2VoaJiUlKR+PNVfjur7+Euq\n7W/fvlV/QI2/Xs2+v5GRkUIIKyurNPcWKlRICPHmzRv1B1T9bX9tvqqhoaFCoVDNbFVTamrq\nZ9NiPiWTyVJT011p8v+7du1aOnvz58//4sULpfKLtY2+7v379+Lrr9fMzCwmJubhw4e1a9dW\nf0wN0vjnGQA+o2/Fztiimur6l5oaNWq0Z8+ehISEdI5ZunTpiRMnHBwcMhfJ3t7+zp07iYmJ\nJiYmn+1KSEh49uxZ6dKlMzeyRqi+Ai9evJjmxMmnT58KIcqWLav+gBYWFlFRUW/fvv2ynbx5\n8yY5OTlv3rzqj1awYEEDA4MHDx6kuVe1Xf0TtEILr1ez76/qj3727Fmaey9duiS+3lrSZGVl\n9eHDh6+dNVSdh27btq36A8rl8qSkpKioqPz583+2KzIyUqlUqqYpqKl37967d+/+2t779++r\n/kT1B7S3tw8PD1edp/xSTEyMEKJWrVrqD6hZGv88A8DnpL4WrAP69OkjhJgyZUrmfvv8+fNF\nuveczZ8/P2sBs0TVg/Pnz//lrsuXL8tkMktLywwN2K5dOyFE3759v9yluj7brl27DA1Yu3Zt\nCwuL58+ff7nLzc3NwMDgs1ka6dP469X4+2tpaSmTydJc+ULVpcLCwtQfbfbs2UIImUz25S7V\nbIyM/kegQoUKQoh69ep9uatu3bpCiAoVKmRoQFUGLy+vr+3y8PBQfzRVTTQ2Nv5yl+oStqGh\nYYbiaZxmP88AJJGT77Gj2H1bFovd27dv7e3tLSwsDh48+On2gwcPWlhY2Nvbv337VhMxM69U\nqVJCiOrVq8fFxX3ceO7cOXNzcyHE5MmTMzTa8+fPDQwMZDLZZyt9BAYGymQyAwODNL/S0nHg\nwAFVvE8XmUtKSlLdaZ6hb30Vzb5ejb+/qtvXzM3NL1y48HFjXFycauaEk5NThkZT/nfaqZGR\n0ad/8/PmzVPVpoyuFffxBsQ+ffp8ul31z0RkfP2Uj2dwf/75548bVes8Z+5/PlWnigsVKvTp\nPOV9+/ap/h569uyZ0QE1S+OfZwDZj2Kn27JY7JRKZVhYmOrumVq1ao0aNcrb21t1McjGxibN\n2ZTZ7OXLl5aWlqqTGWXLlq1Zs+bH685NmjTJxICrV69W3YZlaWnp4uJSrVo11fgymSzNM1vf\nNHnyZJlMZmpq2qFDB19f30GDBqkuR9auXfvdu3cZHU3jr1fj72/jxo1VeYoUKVKzZs2yZcuq\nblu0tLTMxDJsn84VMDQ0NDY2/niTXEbXOlFRzX5VjVawYMGCBQt+vKsyzdmy3/TlYsIf7dix\nI6Oj3b9//2OevHnzFi5c+ONi1BmaYKs9mv08A8h+FDvdlvVip1QqHz165OHhoeoTQggrKysP\nD4+IiAhNhcyid+/etWrV6tObmaytradNm5bpAU+dOuXk5PSxQMhkMicnp6ys43Do0KH69et/\n/MIuUaLEzJkzExMTMzeaxl+vxt/fadOmfTonQy6Xt2rVKtPf+o8fP1adj/yUi4tLpuNt3779\ns3soraysMrpC4ae+fNycgYHBtWvXMjdadHR0mTJlPp3kIZfLc9TJMM1+ngFks5xc7GTKjMw4\nk9ably/iFOpOuCtSpIim/lwPD4+1a9dOmTIlICAgi0OlpKQ8f/5cCFG4cOEM3RKeba5fvx4d\nHe3s7FygQIGsj5aQkPDxWbFprt+RUXFxcZGRkRYWFhqJJzT9ejX+/v7777+3bt3Knz9/huZz\npGPt2rUPHjxo27atarnBLHr//v3Ro0eFEE2aNFE9LSOL9u3bFxISUq5cOdUqvll38uTJ58+f\n16pVK83lVCSn8c8zgOxx6tSpunXrLliwwNPTU+osn8uJ3eJrRlf5ftXL92oenDMLq1wuL1as\nmNQp0lO+fHkNjpYnTx7V4xw0xdzc3NHRUYMDavb1avz9LVCggGa/8j/eCacRFhYWGZpR+02t\nW7du3bq1Bgds0KCBBkfTOI1/ngFAl4rd1CO/l167ZPz8X+MVyrwVGtb5TgNnCAAAAPSGLhW7\nguXq+syu2yjfg2r+55yHLts7SN0lZAEAAHID3XukWIWhc6WOAAAAkBPpXrEztqpb1aGQdZ6v\nPrQKAAAgd9KlS7EfXXzyQuoIAAAAOY7unbEDAABAmih2AAAAeoJiBwAAoCcodgAAAHqCYgcA\nAKAnKHYAAAB6gmIHAACgJ3RyHTudtmXLlvv377dr165cuXJSZwEAAHqFM3bZbePGjQEBAeXL\nl//+++99fX3PnDmTmpoqdSgAAKAPKHbZbefOnUePHh02bFhCQsKsWbNq167t4ODw888/Hz9+\nXKFQSJ0OAADoMIpddpPL5Y0bN160aFFERMT58+f9/f3z5s27fPnyxo0bFy1a1NPT8/Tp00ql\nUuqYAABA91DsJCOTyapVqxYYGHjjxo2bN2+OHz/e0tIyKCioTp06JUqUGDNmzNWrV6XOCAAA\ndAnFLkdwdnaeNGnSnTt3Ll26NGbMGCHE7NmzK1WqVLVq1aCgoFevXkkdEAAA6ACKXc5SpUqV\nmTNnPnz48NSpUwMHDnzw4IGnp2eRIkXat2+/Z8+e5ORkqQMCAICci2KXE8lkstq1awcHB794\n8WLjxo0NGzbcvXt327ZtixUrNnbs2IiICKkDAgCAnIhil6OZmpp269bt0KFDERERgYGB5ubm\n06ZNK1mypLu7+/79+1knBQAAfIpipxscHBz8/f3v3r178OBBd3f333//vXXr1iVLlpw+fXpU\nVJTU6QAAQI5AsdMlBgYGLVq02Llz56NHj8aPH5+UlOTv71+sWLGBAwfeunVL6nQAAEBiFDud\n5ODgMGnSpIiIiK1bt1apUmXlypXlypVr1arVkSNHpI4GAAAkQ7HTYXK5vGPHjqdPnz59+vRP\nP/10+PDhZs2aVapUaf369SkpKVKnAwAA2Y1ipw9q1aq1devWe/fujRw58tGjR7179/7+++9X\nrFiRlJQkdTQAAJB9KHb6o3jx4vPmzXv8+PHUqVPfvXs3aNCgkiVLBgUFxcfHSx0NAABkB4qd\nvrG2th47duzDhw/nzJmTkpLi6elZokSJ2bNnf/jwQepoAABAuyh2+snCwsLb2/vhw4eLFi0y\nNjYeM2ZMqVKlli5dysVZAAD0GMVOn+XJk2fYsGH37t1bsmSJEGLo0KHOzs4bNmxgZWMAAPQS\nxU7/GRsbDxky5N69ewsWLHj//n3Pnj0rVKiwbds2pVIpdTQAAKBJFLvcwszMzNPT886dO/7+\n/hEREZ06dapfv/7FixelzgUAADSGYpe72NjYBAYG3rt3b8iQIWFhYTVq1PDw8Hjx4oXUuQAA\ngAZQ7HKjQoUKLVmy5Pr1625ubmvXri1VqtTEiRNZFQUAAF1Hscu9SpcuvW/fvr179xYtWnTS\npElly5bdtm2b1KEAAEDmUexyu9atW1+7dm3+/PmxsbGdOnVyc3N78OCB1KEAAEBmUOwgjIyM\nvLy87t696+HhcejQofLly0+bNo0V7wAA0DkUO/yHnZ1dSEjIyZMnHR0dx44dW758+aNHj0od\nCgAAZADFDv9PvXr1Ll26NGXKlKdPnzZr1qxfv36vX7+WOhQAAFALxQ6fMzY2DggIuHbtWrNm\nzUJCQsqXL79v3z6pQwEAgG+j2CFtJUuWPHTo0Pr16xMSEtzd3T08PN68eSN1KAAAkB6KHdLT\ns2fPmzdvtm3bdu3atc7Oznv27JE6EQAA+CqKHb6hUKFCu3bt2rp1a1JSUtu2bTt16hQTEyN1\nKAAAkAaKHdTSsWPHK1euNG/efNu2bS4uLmFhYVInAgAAn6PYQV1FixY9ePDgsmXLIiMj69Wr\nN3369NTUVKlDAQCA/6HYIQNkMtngwYMvXrxYvnx5f3//pk2bPnv2TOpQAADgPyh2yLAyZcqc\nOXNmxIgRJ06cqFy58t69e6VOBAAAhKDYIXPy5MmzcOHCHTt2KJXKtm3benl58QgyAAAkR7FD\n5rVr1+7KlSv16tVbuHBhgwYNnj59KnUiAAByNYodssTBweHYsWO+vr5nz551cXE5ceKE1IkA\nAMi9KHbIKkNDwxkzZuzevTspKalp06YzZ85UKpVShwIAIDei2EEz3N3dz5075+zs7Ofn9+OP\nP8bGxkqdCACAXIdiB41xcnI6ffr0Tz/9tHv37jp16ty9e1fqRAAA5C4UO2iSpaXl1q1bZ82a\ndefOnZo1ax4/flzqRAAA5CIUO2iYTCYbPXr0/v37U1NTW7RosXr1aqkTAQCQW1DsoBXNmzc/\nf/58iRIl+vfv7+npycPHAADIBhQ7aIvqlrv69esHBQW1bt363bt3UicCAEDPUeygRba2tocP\nH+7Zs+fvv/9et27dx48fS50IAAB9RrGDdpmYmKxbt27SpEnXrl2rXbv29evXpU4EAIDeothB\n62Qy2fjx4zdu3BgVFVW/fv1Tp05JnQgAAP1EsUM26dq168GDBxUKRZMmTXbs2CF1HAAA9BDF\nDtmnUaNGx44ds7a27tSp06pVq6SOAwCAvqHYIVu5uLicOXOmRIkSAwcOnDhxotRxAADQKxQ7\nZDdHR8e//vqrYsWKkyZNYok7AAA0iGIHCRQqVOj48eN169YNCgoaPHgw3Q4AAI2g2EEaefPm\nPXz4sJub28qVK/v370+3AwAg6yh2kIypqemuXbvatWu3Zs2a7t27p6SkSJ0IAADdRrGDlIyN\njbdu3dq+ffstW7Z07949OTlZ6kQAAOgwih0kZmRktHXr1h49eqgaXmJiotSJAADQVRQ7SM/Q\n0HDt2rW9evXat29fhw4dEhISpE4EAIBOotghRzA0NAwJCenVq9f+/fs7derENVkAADKBYoec\nwtDQcM2aNR4eHnv37u3evbtCoZA6EQAAOkYudQDgfwwMDFatWpWQkLB582ZTU9M1a9YYGPD/\nHgAAqItih5zFwMBg3bp1cXFx69evt7CwWLJkidSJAADQGZwOQY5jZGS0bds2Nze3pUuXjhw5\nUuo4AADoDIodciJjY+Pffvutfv36CxYsmDJlitRxAADQDRQ75FBmZmZ79+6tVq3a+PHjZ8+e\nLXUcAAB0AMUOOZeVldXBgwfLly/v6+u7bt06qeMAAJDTUeyQo9na2h4+fPi7774bMGDAwYMH\npY4DAECORrFDTmdvb//HH3/kzZu3Q4cOYWFhUscBACDnothBB5QqVWrv3r0ymaxNmzbh4eFS\nxwEAIIei2EE31KhRY8uWLTExMW5ubpGRkVLHAQAgJ6LYQWe0bt166dKl9+/fd3d3j4uLkzoO\nAAA5DsUOumTAgAEBAQHnz5/v3LlzSkqK1HEAAMhZKHbQMZMnT+7Tp8/+/fuHDRsmdRYAAHIW\nnhULHSOTyVasWPH8+fPg4OCyZcuOGDFC6kQAAOQUnLGD7lE9TLZcuXKjRo3at2+f1HEAAMgp\nKHbQSVZWVnv37s2bN2+3bt2uX78udRwAAHIEih10VYkSJXbs2JGYmNimTZuoqCip4wAAID2K\nHXRYvXr1li1b9vDhww4dOiQlJUkdBwAAiVHsoNv69u07YsSIv/76a/DgwVJnAQBAYhQ76Lx5\n8+a1bt16zZo18+fPlzoLAABSothB5xkaGm7YsMHZ2Xn06NFHjx6VOg4AAJLRh2KniH+2ff3K\nmdNnrdywNyKOpxHkRtbW1rt377awsOjatevjx4+ljgMAgDR0rNjF3Njd84cG39ma5bX/fujc\no0KIVxdCnAs4/tR7oJ+/78CebZwKlJrw2x2pY0ICTk5OoaGh0dHR7dq1i4+PlzoOAAAS0KVi\n9yFyfwWXDhsO/Bkly6eIvr/Up2nfjX80b/Tzg+T8P/8ydfXa4AnevS2Tn0ztUnXVo3dSh4UE\n3N3df/nll8uXL3t6ekqdBQAACehSsdvTY/DzpFS/zZc+vHr65v2LCW5F1/RofjXRanf47aXT\nxvbtPXDinLX3/1lrrIwf122H1GEhjcmTJ7u5ua1cuXLVqlVSZwEAILvpUrGbfuZfy2Ljpnep\nIoQwMC7gG7pACFGgxtIfilp8PMbGuecsp7zRV+dKlhKSMjAw2Lhxo6Oj47Bhw86fPy91HAAA\nspUuFbv7CSlmBat//KWJVT0hhHXZIp8dVqaouSLhYbYmQ06SN2/eHTt2GBoadujQgSdSAABy\nFV0qdnWsjN8+DFX895dvH4YIIf79O+yzw/beemNsWSN7oyFnqVSpUnBw8JMnT7p06aJQKL79\nGwAA0Au6VOzGdS/5IWpro6ELz9+4d+HE9m7NA+Wm1jG3xwT8dvXjMSeD+y569q5oaz8JcyIn\n6NGjx+DBg48dOzZx4kSpswAAkE3kUgfIgFpzDrQ5UGHPUq8aS72EEAZG+YKv3vj7hzKBHSvt\nrN3MpXSBJ9f+PnEhwtii/IalDaQOC+ktWLDg/Pnz06ZNa9iwYZMmTaSOAwCA1ulSsTM0Kbbj\n5q11i1b8efbiO6PCXUZO7Vgmf+8rf4m2ndYd/+PmaSGEcKzTecmGVTUsjaUOC+mZmJj8+uuv\nVatW7d69+5UrVwoVKiR1IgAAtEuXip0QwtCkcF+fiX0/2WJkWWHtsVtzIu6EP32T16F0me9s\nJAuHnKdkyZIrV67s3Llzt27djhw5YmCgS/ceAACQUXryPWf3XeladVxpdfhSp06d+vfvf/z4\n8dmzZ0udBQAA7dKxM3Yap1AoDhw4kJCQkM4xjx49EkKkpqZmUyZo2qJFi86fPx8QEFC3bt06\ndepIHQcAAG3Rt2KX9PbUd6V/EkK8ePFCneOPHz/epk0bdY58+JC18XRVnjx5Nm7cWKNGja5d\nu16+fNnW1lbqRAAAaIW+FTulMunly5fqH9+oUaM9e/akf8Zu6dKlJ06cKFGiRJbTQTLlypVb\nuHDhgAED+vTps2fPHplMJnUiAAA0T9+KnbFFtbCwz5csToehoaG7u3v6xxw4cEAIwX33uq5/\n//7Hjh3bvHnz4sWLhw8fLnUcAAA0T9/KiszQ0tXV1dXVVeogyImWL19eqlSpMWPG3LhxQ+os\nAABonq6esYt58fDOnfDI12/jPiTI85hb2xZyKuPsaM+sWKTHyspqy5YttWrV6tGjx9mzZ42N\nWe8QAKBXdKzYKRWxW+dPClq96fTtyC/3FipTs1t/z3GenW3k3EGFtLm4uPj7+0+aNGn8+PEz\nZsyQOg4AAJqkS8VOkfTMo3ql0KvRhkb5XBu3qehc0t7OxsREnpKY+ObVy6ptawQAACAASURB\nVIjwG6f/OjvPp+v6Tfv+ObO+sLG+XWWGpgQEBBw6dGj27NktW7Zs2LCh1HEAANAYXSp2Z7xb\nhl6Nrjts4eYZQxzM00iemhS9eebQnhM2NRve/0Zww2wPCN0gl8s3bNhQpUqVXr16/fPPP3nz\n5pU6EQAAmqFLp7X8Q8Mt7Af/tWhEmq1OCGFgbNt93JZlrgXvbwnI5mzQLSVLlpw9e/aTJ09G\njhwpdRYAADRGl4rdtbhki2LfWJpECOFSv0DyB+Y84hsGDRrUunXrdevWbd26VeosAABohi4V\nu7a2pjG3Z7xMSvfRXqnxIVsf5cnbIrtCQYeFhIQULFhw8ODBT58+lToLAAAaoEvFbuzMFomx\nf5Wv2WnDoYtxCuXnu5WJN//a2b+Z87JHbxtOmCBFQOiY/PnzBwcHx8TE9O3bV6n84hMFAICu\n0aXJE069t60833zQ0h09W243NLZ2dCpZOL+NiYmRIikx9tWLB+H3XyekyGSyRkOW7BnqLHVY\n6Ia2bdv269dv9erVK1asGDRokNRxAADIEl0qdkIY9F98xK3nriVrNh84Hnb71uXwG/85yyIz\nMHEoWa5ZoxZd+49oW72ItCmhWxYuXHj8+HFvb+9mzZo5OjpKHQcAgMzTrWInhBBFXNtNc203\nTQhlSvybN+/i4pOMTc0sbfKasigxMsXc3HzlypVNmzYdMGDAkSNHZDI+SAAAXaVL99h9RiY3\nzWtXwKGoQwG7fLQ6ZEXjxo379et37Nix1atXS50FAIDM0+FiB2jQ3LlzixYt6u3t/fjxY6mz\nAACQSRQ7QAghrKysQkJC3r17169fP2bIAgB0FMUO+I+mTZv26dPnyJEja9eulToLAACZQbED\n/mf+/PkODg4jR45kyWIAgC6i2AH/Y21tvXz58tjY2MGDB0udBQCADKPYAf/PDz/80LNnz/37\n94eGhkqdBQCAjKHYAZ9bsGBBoUKFRo4cGRUVJXUWAAAygGIHfC5fvnwLFiyIjo728fGROgsA\nABlAsQPS0LlzZ3d39/Xr1//xxx9SZwEAQF0UOyBtixcvtrCwGDJkSHx8vNRZAABQC8UOSFux\nYsUmTJhw7969qVOnSp0FAAC1UOyAr/Ly8qpateqsWbP++ecfqbMAAPBtFDvgq+RyeXBwsFKp\nHDRoUGpqqtRxAAD4BoodkJ5q1aoNGzbs7NmzK1askDoLAADfQLEDviEwMLB48eK+vr7Pnj2T\nOgsAAOmh2AHfYG5uvnDhwrdv33p5eUmdBQCA9FDsgG9r06ZNhw4dfvvtt0OHDkmdBQCAr6LY\nAWpZsGCBhYXFsGHDEhISpM4CAEDaKHaAWhwcHAICAu7duzd37lypswAAkDaKHaCukSNHOjs7\nBwYGPnz4UOosAACkgWIHqMvY2HjRokXx8fHe3t5SZwEAIA0UOyADmjRp0qlTp507dx44cEDq\nLAAAfI5iB2TM3LlzLSwsPD09mUUBAMhpKHZAxjg4OIwbN+7evXtz5syROgsAAP8PxQ7IsFGj\nRlWoUGHatGnMogAA5CgUOyDD5HL54sWLExISRo0aJXUWAAD+h2IHZEb9+vU7deq0a9eu33//\nXeosAAD8B8UOyCTVLIpRo0YlJydLnQUAACEodkCmFSlSxM/P7/bt20uWLJE6CwAAQlDsgKzw\n9vYuXrz4pEmTXr16JXUWAAAodkAW5MmTZ9asWW/evBk/frzUWQAAoNgBWdOxY8cGDRqsWLHi\n6tWrUmcBAOR2FDsgqxYsWCCE8PLykjoIACC3o9gBWVW5cmUPD4/jx4/v2rVL6iwAgFyNYgdo\nQGBgoLW1tbe3d2JiotRZAAC5F8UO0IACBQqMHTv2wYMHqsuyAABIgmIHaIanp+f3338fGBj4\n4sULqbMAAHIpih2gGcbGxrNnz3737l1AQIDUWQAAuRTFDtCYNm3aNG3adO3atf/884/UWQAA\nuRHFDtCkOXPmCCFGjRoldRAAQG5EsQM0qVKlSj179jx27NihQ4ekzgIAyHUodoCGBQYGmpmZ\njR49WqFQSJ0FAJC7UOwADStSpIiXl9e1a9fWrVsndRYAQO5CsQM0z8/Pr1ChQuPGjYuLi5M6\nCwAgF6HYAZpnaWk5bty458+fz5s3T+osAIBchGIHaMXAgQPLli07a9Ys1isGAGQbih2gFXK5\nfNq0ae/fv580aZLUWQAAuQXFDtCWtm3bNm7ceNWqVTdu3JA6CwAgV6DYAVo0Z84cpVLp5+cn\ndRAAQK5AsQO0qEqVKp07d963b9/JkyelzgIA0H8UO0C7AgMDjY2N/fz8lEql1FkAAHqOYgdo\nV4kSJQYPHhwWFrZ7926pswAA9BzFDtC6gIAAKysrPz+/lJQUqbMAAPQZxQ7Quvz5848cOfLO\nnTuhoaFSZwEA6DOKHZAdfHx8ChYsOG7cuPj4eKmzAAD0FsUOyA4WFhZ+fn7Pnj1bunSp1FkA\nAHqLYgdkkyFDhjg6OgYGBsbExEidBQCgnyh2QDYxNjaeMGFCTEzMvHnzpM4CANBPFDsg+/To\n0aNSpUrz589/+fKl1FkAAHqIYgdkHwMDgylTpsTFxU2dOlXqLAAAPaRWsUtNjho5cuTM7RHa\nTgPoPXd39wYNGqxYseLevXtSZwEA6Bu1ip2BUf7fVyxZvOymttMAucHUqVOTk5MnTJggdRAA\ngL5R91Ls2tH1Is+MvPmBdfOBrKpbt66bm9uWLVuuX78udRYAgF5Rt9jVnHh0k2+1xhVazF23\n89L1248iPqfVlICemTJlilKpHD9+vNRBAAB6Ra7mcUZGRkIIpULh0+dYmgcolUqNhQL0nYuL\nS7t27Xbu3Hnu3LkaNWpIHQcAoCfULXb9+/fXag4gt5kyZcru3bsnTpx44MABqbMAAPSEusVu\n2bJlWs0B5DblypXr0qXLpk2b/vzzz/r160sdBwCgD9Qtdh89uXnu7OVbUW/i8ljblqlcs1b5\n77QRC8gNJk6cuHXr1oCAgD///FPqLAAAfZCBYvf66o7eHp77Lj39dGORqq0Xr1vfrnxeTQcD\n9J+Tk1OvXr1CQkKOHDnStGlTqeMAAHSeusUuPmpPFdfOTxJTXd37tG3iWjS/5YfXz84d2bV2\nz/6O1avtfXKjpV0erQYF9NKECRM2btzo7+/fpEkTmUwmdRwAgG5Td7mTvV2HPklUBuy+E7Zn\nzS+eg3t06z5w2JhVu07f2TtemfhwYPd9Wk0J6KtixYr179///Pnz+/fvlzoLAEDnqVvsZpz9\n18Zp+mT3Up9tL/nDxDll8kWenq7pYEBuMXbsWDMzM39//9TUVKmzAAB0m7rFLjw+xcqpapq7\nKjtbp8SHay4SkLvY29sPGTLk2rVr27dvlzoLAEC3qVvsXCyNXl/ZmeauvRdeGVtW11wkINfx\n9fW1tLScMGGCQqGQOgsAQIepW+zG//jdu2dLfpy2O+X/PWBCsW9mx3mP337341gtZANyCzs7\nO09Pz1u3bm3ZskXqLAAAHaburNj6i3c02l9j19h2Bda4tm7iWsTW7EP0s3NH94XdizHN32j7\nYpZXBbLE29t78eLFkyZN6ty5s1ye4QUmAQAQ6p+xk5uVOxh+/pfeLZURF0KDg2ZMmxEUHHou\nIrVFL9/z9w6WM+N7CMgSGxub4cOHh4eHc9IOAJBp6hY7IYSxVdlpa39//eH1jUvnTh4/ee7S\n9Zi41wfXzShnZay9fEDu4e3tnTdv3smTJ6ekpEidBQCgkzJQ7FRkcquyVarXb1i/epVyVkYZ\n/u0Avsba2nrEiBHh4eGbN2+WOgsAQCeldwn12bNn6g9UpEiRLIcBcruRI0cGBQVNnjy5a9eu\n3GkHAMio9L45HBwc1B9IqVR++yAA6bK2tvb09Jw4ceKmTZt69eoldRwAgI5Jr9j1798/23IA\nUPHy8lq4cOHkyZO7devGSTsAQIak97WxcuXKbMsBQMXa2trLy2vChAkbNmzo06eP1HEAALok\nw7Mfntw899vGdcuWLF2z4dcz1yO0kQnI5by8vPLlyzd16lSmxwIAMiQDxe711R3uLkWLlXPt\n2KPPkGFD+/bsUrtCcQcX913XY7SXD8iFrKysvLy87t+/HxoaKnUWAIAuUbfYxUftqeLaef/l\n567ufaYtWBa6cUPwopn92tZ6eXl/x+rVDr5K0GpKILcZOXKknZ3dpEmTkpKSpM4CANAZ6t6a\nvbfr0CeJyoDddya7l/q4ceCwMb/sn1jaffLA7vseH/pJOwm/pIx68j5/Ucv//jL1n5P7/7x4\n832qSYmy1Vu1qG1lKMuuJIC2WFhYjBw5cuzYsaGhof369ZM6DgBAN6h7xm7G2X9tnKZ/2upU\nSv4wcU6ZfJGnp2s6WNoeHV5av0z+Mk1/U/0y/t+THaoUrtywzQhvP//RI7v+UNe+aLXgky+y\nJwygVcOHD8+XL9/06dO50w4AoCZ1i114fIqVU9U0d1V2tk6JD9dcpK96dXmus9uwU/cSqzcs\nKoRQKt51rvLDjiuRFd36TJ2zeO2KIJ8BP6RGXh7arNLW53HZkAfQKktLS09Pz/v372/atEnq\nLAAA3aBusXOxNHp9ZWeau/ZeeGVsWV1zkb5qcefAJJnZqrAHB4ObCiFe/N1/7/O4qmP2/XNg\nzVjvob0HDJ+9Yl/E6UVyxSuvzjuyIQ+gbZ6enjY2NlOmTOGkHQBAHeoWu/E/fvfu2ZIfp+1O\n+X8PmFDsm9lx3uO33/04VgvZPrfk0du83y/0qJZf9ctHm64KIVaPb/7pMQVch84tne/VpRnZ\nkAfQNmtr6+HDh9+7d2/btm1SZwEA6AB1J0/UX7yj0f4au8a2K7DGtXUT1yK2Zh+in507ui/s\nXoxp/kbbF9fXakqVfHKDNyYf50wIA2MDIUQxk89fgmP+PIpwbrODnvDy8lqwYMGUKVM6d+5s\nYJDhhScBALmKut8TcrNyB8PP/9K7pTLiQmhw0IxpM4KCQ89FpLbo5Xv+3sFyZtnx4COvcnlf\n3xp9NvY/qz+U7FNPCDH54r+fHqNMiQm88srUtnU25AGyQb58+YYOHXrr1q3t27dLnQUAkNNl\n4ASAsVXZaWt/f/3h9Y1L504eP3nu0vWYuNcH180oZ2WsvXyf6rYx0CjlSWPnxku2/xWbkprf\nZcnoOoWWt2i95sQD1QEfXpwf2abKqbeJDcb/kj2RgGzg4+NjYWExadKk1NRUqbMAAHK0jFzZ\nUSb+sX6h77jDZatUr9+wfvUq5bp06TV9xZ64VOW3f68mWH/f//K2iTavw4b9VN/WwrZM5drn\nDAsnxl7o26ikZYHvnEsUsiriuvD3iDoDFuz+2Tl7IgHZwNbWdvDgwTdu3NizZ4/UWQAAOZq6\nxS41+d+BNYs17+21fMO5jxsP7dzkP6ht8ep9XiZn04mE0j+Of/Di6ryAobXLFHh+6+LJPy+p\ntr+PevwiPk+TToNCT9z7e4WnnCWKoV98fHzMzMwmT56sVGbT/0cBAHSRusXu0sRWK8/9W73X\n1MNHxnzc+ObJtTkD6726tL657xntxEuDSd6yI6cs/vPKnbcJ8dH/Pn8cEfHsReSbd0lvXj46\ntGVZjwYlsy0JkG0KFiw4YMCAy5cvHzhwQOosAICcS91iNzn4pnmBHmfWja1Z2u7jRssi5byD\nTwy2twhfO1k78dIlM8qX375osWKFCxWwtjCSIACQjXx9fU1NTSdMmMBJOwDA16g7m/VkbGK+\nxr0M09hj0LG63Yr9pzUZKhspFIoDBw4kJCSkc8yjR4+EENy3DmnZ29t7eHgsXbr08OHDLVq0\nkDoOACAnUrfYOZsZ3X10RYhmX+66Gv5OblZao6kyL+ntqe9K/ySEePFCraXsjh8/3qZNG3WO\nfPjwYZaSAVnm5+e3atWqKVOmUOwAAGlSt9hNbFW01a9+vltbzuxU4dPtd/aM97n92uGHVVrI\nlhlKZdLLly/VP75Ro0Z79uxJ/4zd0qVLT5w4UaJEiSynA7KkaNGivXv3Xrly5cmTJxs0aCB1\nHABAjqNusWuyamudI3Vmda64b2n7Nk1cHewsEmIjL53Yt/nwZblZuXUb3LSaUn3GFtXCwsLU\nP97Q0NDd3T39Y1S3q7PoP3ICPz+/NWvWBAYGUuwAAF9St9gZmVc5cvuU74Bhy3bvnHFyx8ft\nZRv3WBSyrIG1iXbiZZjM0NLV1VXqFIC2ODo6dunSZcOGDWfOnKlVq5bUcQAAOUsGHgVmYltl\nwY5TM149Cjt/7eXrt8aW+cpUrlm2WF7thUtHzIuHd+6ER75+G/chQZ7H3Nq2kFMZZ0d7G0nC\nANnJ399/06ZNM2fO3LVrl9RZAAA5S4af8ZrHrnhDt+JaSKIWpSJ26/xJQas3nb4d+eXeQmVq\nduvvOc6zsw0rFEN/OTs7t2vXbufOnVevXq1YsaLUcQAAOUh6xS42NlYIYW5lLZf95+d0WFtb\nazJXWhRJzzyqVwq9Gm1olM+1cZuKziXt7WxMTOQpiYlvXr2MCL9x+q+z83y6rt+0758z6wsb\nc0sc9Na4ceN27tw5ffr0zZs3S50FAJCDpFfsbGxshBDbX31ob2uq+jkd2bBo6hnvlqFXo+sO\nW7h5xhAH8zSSpyZFb545tOeETc2G978R3FDbeQCpVK5cuWXLltu2bZs4cWLp0jllsSEAgOTS\nK3ZdunQRQjgYy4UQPXr0yKZEX+cfGm5hP/ivRSO+doCBsW33cVveHzjpuSVABP+dndmAbDZ+\n/Pjff/995syZISEhUmcBAOQU6RW7T6/yhIaGaj/MN1yLS7Yo842lSYQQLvULJF+4kQ15AAnV\nrFmzYcOGGzZsGD9+fPHixaWOAwDIETJ/I1pC1LU9v24+ceFOSnY9uLKtrWnM7Rkvk9J9tFdq\nfMjWR3nysi4/9N/YsWOTk5PnzJkjdRAAQE6hfrFT/jZ9cM0KJVe+jBNCvItYX7pY1bZdujWq\nXsax4YiYbCl3Y2e2SIz9q3zNThsOXYxTfPEnKhNv/rWzfzPnZY/eNpwwIRvyANJq2rRp7dq1\nV69e/fz5c6mzAAByBHWL3Z2VbTv6B1+4+9rUQCaEWO4+6mmyyYjA+aN7Vn3y5yL3ede1GfI/\nnHpvWzm0ScyVHT1bVrM2y/t9eZeGjZq0aNmyaeNG1SuWsTWzKFe/fcjxx42GLNkz1Dkb8gCS\n8/X1TUhImD9/vtRBAAA5grrr2E0fd8zYvOLZp+cr2xgrEh9NvBnj0Py3hf4/CuH57JDF7vnz\nxZhsuIPboP/iI249dy1Zs/nA8bDbty6H3/jPeTuZgYlDyXLNGrXo2n9E2+pFtJ8EyBHc3d0r\nVqy4fPlyPz8/W1tbqeMAACSm7hm7ndHxdlVnVLYxFkK8jZj3QZFaI0D1OCOZR1W7+OjdWkv4\nuSKu7aYt//XKnYj4xLjXUZFPHj+JjIqOS4x/fPfKluCZtDrkKjKZzN/f//3790FBQVJnAQBI\nT91iZyKTif/e1XZ/9UmZTDaqQj7VLxUpSqFM0Ua49MnkpnntCjgUdShgl8+UR00gt/rpp5+c\nnJwWL1787t07qbMAACSmbrHrVcj81T/jIxIVSsXbCavCzQr0rGVpLIRITXo+9mykiU0TbYYE\n8FWGhoajR49+/fp1cHCw1FkAABJTt9gNW9A26d2FsiUquJb77sDr+Bq/jBFCPN0/2716xYvv\nkpz7/aLNkADS06dPn2LFis2dOzc+Pl7qLAAAKalb7Iq3X380aHBRgxcX7ydX6zh217CyQojn\nR9YfuBpd1m3UoSku2gwJID1GRkaenp4vX75cv3691FkAAFLKwALFjYcvu/00JiHp/fmtU60M\nZUKI0gOWX7j98saBuXZGmV/oGEDWDRo0KH/+/LNmzUpJkeCGVwBADqFWIUtNjho5cuTM7RFC\nCKNPZilYl63jUrqglpIBUJ+5ufmwYcMePHjw66+/Sp0FACAZtYqdgVH+31csWbzsprbTAMi0\nYcOGWVpaBgYGpqam+9g9AID+UvcS6trR9SLPjLz5gas8QA6VL1++QYMG3bp1a9++fVJnAQBI\nQ91iV3Pi0U2+1RpXaDF33c5L128/ivicVlMCUIe3t7epqWlgYKDUQQAA0lD3kWJGRkZCCKVC\n4dPnWJoHKJXKNLcDyDaFChXq1atXcHDw8ePHGzVqJHUcAEB2U7fY9e/fX6s5AGjEmDFjVq9e\nPX36dIodAORC6ha7ZcuWaTUHAI1wdHTs3Lnzxo0bw8LCatasKXUcAEC2ytj6c6kpr//+ffvS\nhXOnB04VQsQ9imD2HZDT+Pn5yWSy2bNnSx0EAJDdMlDsXhxfWrNo0Xqtfhrq5eMfME4IcWVS\ni3wlqgcdfqy1eAAyrHz58u7u7rt27bp5kyWKACB3UbfYvX/6a5WWIy6+Mu7mFRA4qqxqY5FW\nHfL9+8/IHyqsefhWawkBZJi/v39qaurcuXOlDgIAyFbqFrutnb2iFHnWXX24cf6Uns2LqDYW\n7xj4z/XfrMR7/25btZYQQIa5urrWr18/NDT08WNOqANALqJusZt5OTpfuYU9nG0+225Zos3i\n8nbRVzkxAOQsfn5+ycnJCxculDoIACD7qFvsIpMV5g7F09xlX8xMkfRcY4kAaIKbm1vVqlWD\ng4NfvXoldRYAQDZRt9i1zJvn1cV1aa1BnLr2bJSJdQNNhgKgCT4+PnFxcUuWLJE6CAAgm6hb\n7PxHVYmLDG3qGxKX+km7UybvnOgWGhn3fd+xWkkHIAs6derk5OQUFBT0/v17qbMAALKDusWu\nwuj9w2oWPDarXwGHMj0mXRFCDPDoXuv7/O0nHbZ26rhvajVthgSQGYaGhqNGjXr9+vXq1aul\nzgIAyA7qFjuZoXXQ3/fWThlaUv7vn2eihBCr1m66EpO326i5N69vcTA21GZIAJnk4eFhb28/\nZ86cpKQkqbMAALQuAwsUywwtegcsvvo4JvrZo5vXrt+PeB736uHGuaMKG2fs8RUAso2JiYmn\np+fTp083bdokdRYAgNal18mex6WkuT1f4e+cy5dzLGZPoQNyvp9//tnGxmbmzJmpqTwCEAD0\nXHrdrKhN/obt+y/feiQ6ie8DQFdZWVn9/PPPt2/f3rNnj9RZAADalV6xK2QSd3Ln6p87Nytk\nXdit+/B1e0+9V6S14AmAnM3T09PU1HTGjBlSBwEAaFd6xe7pm1d/71k3snebwsYxBzct7tOm\nrm2+4u37j9l65FISBQ/QHQULFvTw8Dh79uzx48elzgIA0KL0ip1MblXHvde8tbsfxcScO7h5\nzICfihn9u3P17M7NXGwKlu4+bPzeU7e4RgvoBB8fH7lcPnPmTKmDAAC0SK35DzIDs+otusxc\nsS08Kvbyse0BQ7oWN3y6acmUNnXL2hSr3G/09COXHmk5J4AsKVGiRKdOnQ4dOnTp0iWpswAA\ntCWDE1tlxpUbtZ+yZNPN57E3/toz2at3ceW9kDn+zVxKaCceAI3x9fWVyWSzZ8+WOggAQFsy\nu2KJMjku7n1MzOvXMax6CuiGihUrtmzZctu2bffu3ZM6CwBAKzJY7JQp1//cFTCkS6kC+Wq0\n7DZ/3d7XZo49PCf/fu6+duIB0CRfX1+FQjFv3jypgwAAtEKu5nEPLhzevHnzli3brj+PE0LI\n8xR06zGoR48eHZpXM5FpMyAAzWnQoEHt2rVDQkLGjRtnb28vdRwAgIZ9o9i9uPHXli1bNm/e\ncv7+ayGEzNDUtVXP7t27d/2pmR1PEgN00OjRo3/88cdFixZNmzZN6iwAAA1Lr9g1qFDkz+vP\nhRAymayUa6sePXp069reydYku7IB0Ly2bduWK1duyZIlvr6+1tbWUscBAGhSemfd/rz+3Nap\n5vCJi8LuvgoP2z9hWFdaHaDrZDKZj4/P27dvg4ODpc4CANCw9Ird72fvvbp7JmjCsBql8mVb\nIADa1r1792LFis2bNy8hIUHqLAAATUqv2LWsUTKdvQlR1/b8uvnEhTspPF4M0ClGRkZeXl6R\nkZGhoaFSZwEAaJL6EyCUv00fXLNCyZUv44QQ7yLWly5WtW2Xbo2ql3FsOCKGcgfolIEDB9rZ\n2c2cOVOhUEidBQCgMeoWuzsr23b0D75w97WpgUwIsdx91NNkkxGB80f3rPrkz0Xu865rMyQA\nDTM3Nx8yZMj9+/d37twpdRYAgMaoW+ymjztmbF7xQmRkjwJmisRHE2/GODQPXejvNWv9hW4F\nzK7Mn6/VlAA0bvjw4WZmZjNnzpQ6CABAY9Qtdjuj4+2qzqhsYyyEeBsx74MitUZALSGEEDKP\nqnbx0bu1lhCAVtjZ2fXr1+/ChQtHjx6VOgsAQDPULXYmMpn4731091eflMlkoyr8Z6qsIkUp\nlCnaCAdAq0aPHm1kZMRJOwDQG+oWu16FzF/9Mz4iUaFUvJ2wKtysQM9alsZCiNSk52PPRprY\nNNFmSABaUbRo0c6dO//xxx8XL16UOgsAQAPULXbDFrRNenehbIkKruW+O/A6vsYvY4QQT/fP\ndq9e8eK7JOd+v2gzJABt8ff3NzAwmDVrltRBAAAaoG6xK95+/dGgwUUNXly8n1yt49hdw8oK\nIZ4fWX/ganRZt1GHprhoMyQAbXF2dnZzc9u+fXt4eLjUWQAAWaX+Onai8fBlt5/GJCS9P791\nqpWhTAhResDyC7df3jgw184oA+MAyFF8fX0VCsXcuXOlDgIAyKoMFzIj2f9+ti5bx6V0QU3G\nAZDt6tWrV6dOnbVr17548ULqLACALEmv2MXGxsbGxqoeKhH7LdmUF4AWjBkzJjExMSgoSOog\nAIAskaezz8bGRgix/dWH9ramqp/ToVTyVDFAV7m7u5cvX37p0qW+vr7f/McOAMix0it2Xbp0\nEUI4GMuFED169MimRACynUwm8/Hx6dOnT3BwsK+vr9RxAACZlF6x27x588efQ0NDtR8GgGS6\ndes2YcKE+fPne3p65smTR+o4AIDMUGPyhDLpzsXTl2IS/7dBEbt+kY9BOQAAIABJREFU7i8d\n27Zya9vRc9Kiy1EJWgwIIFsYGRl5eXlFRkauX79e6iwAgEz6RrGLvryhbkm7MtXqjL0a/Z9N\nysRRtb/v7TNj+97Dfx/aGTRxRI1iZRadj9J6UgBaNmDAAFtb2zlz5igUCqmzAAAyI71ilxJ/\np27dvueiCnhOme9XNq9qY8SeHgvO/Vuw9qi7bz68i/9w80RIdfN/RzdtE5fK5AlAt5mbmw8b\nNiw8PHzHjh1SZwEAZEZ6xe7+pv53EuRrr11eEODVIL+pamPIqCMyA6NN+6eVsjIWMmPnBh77\n/w5IfBv2y31WPAF03ogRIywsLKZNm8Y8dwDQRV+dPHHz9z2bF9wysaplcvHw9v8+H1ypeDfj\nUWweG7eYo/u2//dIZWoxI5ns6JKN2+sVMi9cv2Wt/FpPDUA78uXL169fv4ULFx45cqRZs2ZS\nxwEAZMxXi92BSX7L7rxJSj3r5XX348bU5NdJqUrj1EteXlc/PThViHtrp3htNyra4leKHaDT\nfHx8li1bNnPmTIodAOicr16K9Qm7uae3k4Hc9k7E4yf/tatXKSHEwD+vP/nEg1u7lULU23j2\nyZMnp1fVzsbwADTPwcGha9euR48eDQsLkzoLACBj0rvHruyYLimJj90m70kVQggRe2f3j0E3\njC2qziiX79PDfvPuoRTiZ9cC2swJIPv4+fkZGBjMmTNH6iAAgIxJr9jZOE2Y1br4n5PaFShV\nsV6tKg7l2j9PSu29Ypu5gUwIkfT29NTxY35qXKbbilvFWi/tYGeaXZkBaFeZMmVat269c+fO\nmzdvSp0FAJAB31jHzmf39TVThpQ2S7j3MMqxRssZv15c0dVRtSsh5sC4KbN3/hX5w8BZF3YM\n1H5UANnH398/NTV13rx5UgcBAGRAeo8UE0LIDMz7BCzpE5DGLovCw67f6OlQysnaWI3HVwDQ\nKa6urvXr11+/fv2ECROKFi0qdRwAgFoy38mS3kTdv3bp8tXwFJa7AvSRr69vcnJyUFCQ1EEA\nAOpSv9gpf5s+uGaFkitfxgkh3kWsL12satsu3RpVL+PYcEQM5Q7QO61atapSpcqyZcuio6O/\nfTQAIAdQt9jdWdm2o3/whbuvTQ1kQojl7qOeJpuMCJw/umfVJ38ucp93XZshAUjDx8cnLi5u\n2bJlUgcBAKhF3WI3fdwxY/OKFyIjexQwUyQ+mngzxqF56EJ/r1nrL3QrYHZl/nytpgQgiU6d\nOjk6Oi5atOjDhw9SZwEAfJu6xW5ndLxd1RmVbYyFEG8j5n1QpNYIqCWEEELmUdUuPnq31hIC\nkIxcLvf29v73339Xr14tdRYAwLepW+xMZDLx3/vo7q8+KZPJRlX4zzLFihSlUKZoIxwAyfXt\n29fe3n727NlJSUlSZwEAfIO6xa5XIfNX/4yPSFQoFW8nrAo3K9CzlqWxECI16fnYs5EmNk20\nGRKAZPLkyTN8+PAnT55s2bJF6iwAgG9Qt9gNW9A26d2FsiUquJb77sDr+Bq/jBFCPN0/2716\nxYvvkpz7/aLNkACkNHToUBsbm+nTp6empkqdBQCQHnWLXfH2648GDS5q8OLi/eRqHcfuGlZW\nCPH8yPoDV6PLuo06NMVFmyEBSMnKymrw4MG3b9/es2eP1FkAAOnJwALFjYcvu/00JiHp/fmt\nU60MZUKI0gOWX7j98saBuXZGPHwC0GcjR440NTWdNm2a1EEAAOnJcCEzkv3vZ+uydVxKF9Rk\nHAA5UoECBTw8PM6fP3/s2DGpswAAviq9YhcbGxsbG6t6qETst2RTXgAS8fHxkcvl06dPlzoI\nAOCr5Onss7GxEUJsf/Whva2p6ud0KJU8VQzQZyVKlOjSpcuGDRvCwsJq1qwpdRwAQBrSK3Zd\nunQRQjgYy4UQPXr8H3v3HdfE+YAB/L2QsPeUISiggKKoKCDiwIqCC1etVnFitS4UUXEgzgIO\n6t64cBXq3qMuBLXuAYI4ABVQlL0hye+P9JdSlgGTXMbz/cMP3l1enstx4eFydxktpkQAIKkW\nLlx4+PDhtWvX/vnnn3RnAQCAWtRX7I4cOcL/OjIyUvRhAECi2dnZ9evX78SJEwkJCa1ataI7\nDgAAVCfQxROciqzZs2eHHUsVdRoAkHCLFi3icDhr166lOwgAANRCoGLHYBlc2Lll87YEUacB\nAAnn7OzcvXv3gwcPpqbiLz0AAIkj6O1O9s3t+unO7IRifCYsgLxbuHBhRUVFeHg43UEAAKC6\n+s6xq8pl6V+HGaN7tukzd8l0d0c7XQ0V6r8LWFhYCD0cAEig3r17Ozo67t69e/HixQYGBnTH\nAQCAfwla7FgsFiGEy2YHjKv99qS43QmA/FiwYMGwYcM2bNiwcuVKurMAAMC/BC12vr6+Is0B\nAFJkyJAh9vb2mzZtCggI+OZNLgEAQGwELXbbtm0TaQ4AkCIURc2dO3fs2LFbt25duHAh3XEA\nAOAfDf6sWAAAQsioUaOsra3Dw8MLCwvpzgIAAP+Qnc+K9fHx8fvtOd0pAOSFgoKCv7//169f\nIyIi6M4CAAD/qK/YaWtra2trn84u4X9dD3EFrtPBgwePXUmnOwWAHJkwYYKJicnatWvLy8vp\nzgIAAIRI12fFvj20PvJ1fYcGC1IOLVt2l/d1cHCwWEIByC8lJaXZs2fPnTv3wIEDuL4KAEAS\nUEK+TQm33K6Vw8uXL4U55v/dGGrlfvytoEGEt17jx4/ft2/fihUrFi9eLKwxAWRDUVFRs2bN\ntLS0EhMTmUxBL8YCAJBqsbGxbm5u69ev9/PzoztLdUJ/IeYmJiYKe8x/dDsSGzr1p8CIW8q6\n7VZuWmyt9p/wgwYN0rMPjljZXkTfHQBqUlNTmz59+tKlS//880/eMX4AAKCRNP2FzVBsMn/3\nzb59w4aODVrs91v44ehfPSyrLqCs39nbuw9d8QDk04wZM9atW7dixYrhw4czGLjQHgCATtL3\nKtxmyPznKffGOWRP69PSa+bGr5UcuhMByDVdXd0pU6YkJCScO3eO7iwAAPJO+oodIURJr/22\nq29OrZ0Ut222lZ3Xn0++0J0IQK75+/srKyuvWrWK7iAAAPJOKosdIYQQxgD/balPTrgq/P1T\nR4txq/6gOw+A/GrSpMmECRPu3bt35coVurMAAMg16S12hBCi3XrguRdv1k/tFhk0ku4sAHIt\nMDBQUVERB+0AAOglTRdP1Ipi6s7YeKHvgANnE3LUzezojgMgp5o2berj4xMREXHr1q1u3brR\nHQcAQE5J9xE7PiuPMX5+fhOHmtMdBEB+LViwgMlk4qAdAACNZKTYAQDtrKysRowYcfny5bi4\nOLqzAADIKal/K7aa8vxYC5thhJCMjAxBlmez2efPny8tLa1nmZSUFEIIh4P7qgB8w6JFiw4f\nPhwSEnLmzBm6swAAyCNZK3ZcbnlmZqbgy1+/fn3gwIGCLPnu3bvGhgKQF7a2tkOHDo2Ojn74\n8KGjoyPdcQAA5I6wix3FXLt2rZDHbAhF9Y53794VfHl3d/fTp0/Xf8Ru69atN27caN68+Xen\nA5B9wcHBx44dCwkJ+fPPP+nOAgAgdwQtdlx24a7ACRuir7/9VFjrAiUlJYQQQhTmzJkjpGyN\nQSloODs7C768goLCgAED6l/m/PnzhBB8VhKAIFq3bt2/f//jx48/f/68TZs2dMcBAJAvgha7\n2wFdJ69/oqBk2MGps5aSgkgzCSIn411SUvKn7Pyi4lKmspqWXpMWtnaWxtp05wIAEhwcfObM\nmdDQ0EOHDtGdBQBAvgha7Obveamo3i727Z2OBsoiDVQ/Ljsv6vdlGyMOxyV+qjm3ia3Lz75+\nQX4/aTMp8WcDAJ4OHTr06dPnjz/+WLJkiY2NDd1xAADkiEDFjsspuV9Q3vznjfS2Onb5x/Gd\nHCKffVVg6Tr3HNjWzspYX1tJiVlZVpb7JTM1OT4u5l54wMgDh88+vXPARBHvnALQZsmSJRcv\nXgwLC9uzZw/dWQAA5IhgxY5dxCWES/f9Pu7M8Yx89tVt+oYjoVPN1GpJzin/eiRsmk/wYY8Z\nvvE7eog9IAD8o3Pnzu7u7gcPHgwKCsKFRwAAYiPQYS0GS3+5k2Ha6ZkvCitEHageCyOT1Y2n\nxGyaWWurI4QwFPVGBR3d5mz05uhiMWcDgGqCgoIqKipCQkLoDgIAIEcEfb9y3rXrI9tmudj1\nXL0nOvbBizc1iDQlz/OiCnXzb1zBSghx7GZYURwvhjwAUA93d/du3brt27ePd4tvAAAQA0Ev\nnmCptSKEEJIxf+LtWhfgcrlCilQnbz2Vo4mhmeWeTeo5f45TsicqRVnHS9RhAOCbgoKCPDw8\nwsLCtm3bRncWAAC5IGixmz59ukhzCGJRWJ/9447buwxfH7JgcK8Oagr/vfSVW5Zw+3z40tkR\nKfl9NwfTlBEA/tWrV6+uXbtGRETMnz+/WbNmdMcBAJB9gha7TZs2iTSHIFqMjd51v/fkrcd9\nPI8pKGpZtrAyMdBWUmKxy8vyvmS8TX6TXVpJUZT71C2np9nRHRYACCFk8eLFffr0Wb169dat\nW+nOAgAg+6Trs2IZvpuvevmc3LL3yPnrdxNfPk6O/+f9X4qhZGbV2sO9z0jfmd6dTOlNCQB8\nvXv3dnNz4x20s7CwoDsOAICMq6/Y5eXlEULUNLWY1D9f10NLS0uYuepm6jzoN+dBvxHCrSzJ\nzS0oKilXVFHV0NZRwU2JASTS4sWLPT0916xZs3nzZrqzAADIuPqKnba2NiHk2JfiIXoqvK/r\nIYaLJ6qhmCo6+io6Yv6uANBAffr06dKly+7duwMDA83MzOiOAwAgy+ordiNGjCCEmCkyCSGj\nR48WUyIAkDmLFi3q27dvWFiYJJytCwAgw+ordkeOHOF/HRkZKfowACCbvLy8nJ2dd+3aFRgY\naGqKs2ABAERFCB+omri9p65Rp+8fBwBk2JIlS8rKylavXk13EAAAWdaAq2JTr+zbfOJ6Slbx\nfydz4i/F5pd94ww8AJBzffv2dXJy2rlz57x583DQDgBARAQtdunXA208V5dxarlCgqXeZNDc\nA0JNBQAyaMmSJf3791+zZs369evpzgIAIJsEfSt258TtFQo6B+69Li7IWtRGz9T9aGlpaUFW\nyroxrVQM3Xcs/UGkKQFABvTt27djx447duxIT0+nOwsAgGwStNjtzSjStVnn42Sloq4/bn7r\nr0/3KSkpqetbzN5zt1P2iQFhz0WaEgBkAEVRy5YtKy0tDQkJoTsLAIBsErTYZVWw1Sya8r7W\nc7Ipy71exOESQigFjeD+TZ+sXyaqgAAgQ/r27evi4rJz586UlBS6swAAyCBBi107NcX8pGe8\nr5V1enE5ZQc//XMVhYqxSlnOVZGkAwCZs3z58vLychy0AwAQBUGL3RxXo9y38xdG/pVdwVHW\n7WesqLBxVQwhhHArj55IY6q0EGFGAJAhHh4e3bt337t379u3b+nOAgAgawQtdn0PbLVQ5IaM\n6TUqLpNiqP3u1fTl1r4ufYb0dmm27nWuxaAVIk0JALJk5cqVFRUVK1eupDsIAICsEbTYqRj0\ni38Ts2TWpG4GKoSQoUcujOrS/N7lE1cfZHUYtuCv3X1EGRIAZIqbm1vPnj0PHDiQlJREdxYA\nAJnSgE+eUDVxWfb7zgWtdAghTBXbyJg3eZ/ffyksfhj9W1MlBZElBAAZtGLFCjabjYN2AADC\nJVCx41RkzZ49O+xYarXpmgZmuiqodADQYK6urn369Dly5MjLly/pzgIAIDsEKnYMlsGFnVs2\nb0sQdRoAkB8rV67kcDjLluFmSQAAQiPoW7H75nb9dGd2QnGlSNMAgPzo2LFjv379oqKinj59\nSncWAAAZIWixc1n61+H5HXu26bNu/4lHLxJTUqsTaUoAkEm8w3XLly+nOwgAgIxgCrgci8Ui\nhHDZ7IBx12pdgMvlCi0UAMiHDh06DB48+MSJE48ePerQoQPdcQAApJ6gxc7X11ekOQBAPi1b\ntuzkyZOLFi26cOEC3VkAAKSeoMVu27ZtIs0BAPLJ3t5+5MiRhw4dunHjRo8ePeiOAwAg3QQ9\nx+7hw4fJeeW1zipKjX/8NFl4kQBAvqxcuVJRUXHx4sV0BwEAkHqCFruOHTtOuZFe66ykXaM7\nOXUXXiQAkC/NmjWbOHFibGzsuXPn6M4CACDdvvFW7L4tm/IqObyv35/ZuyFFt/oS3MrYo+8I\nURJFOACQE0FBQfv371+wYIGXlxeD0YBPxAEAgKq+UexWBPi/Lf3n3nXJEctn1bFYs747hZoK\nAOSLsbHx9OnTV69eHRUVNWLECLrjAABIq28Uu8jzl0o4XEJIr1692i87uKZLk1qGUNVzdm4n\nknQAIDcCAwN37dq1ZMmSoUOH8u6vBAAADfWNYufq3pP3haenZzuPXj90NhJ9JACQRzo6OrNn\nz16yZMm+ffsmTZpEdxwAAKkk6LksFy5cCEGrAwBRmj17tpGR0dKlS0tKSujOAgAglXCSMgBI\nCnV19QULFqSnp+PGmQAAjYNiBwAS5Ndff23evHlISEh+fj7dWQAApA+KHQBIEN6dir98+RIe\nHk53FgAA6YNiBwCSZezYsXZ2duvWrfv06RPdWQAApAyKHQBIFgUFhZCQkMLCwuXLl9OdBQBA\nyqDYAYDE8fb2dnd337lz58uXL+nOAgAgTVDsAEAShYaGstnsJUuW0B0EAECaoNgBgCRycnIa\nPHjwn3/+GRcXR3cWAACpgWIHABIqJCSExWIFBgbSHQQAQGqg2AGAhGrZsuXEiRNjYmLOnDlD\ndxYAAOmAYgcAkmvZsmUaGhrz5s2rrKykOwsAgBRAsQMAyWVoaOjv75+YmLh//366swAASAEU\nOwCQaAEBAU2aNFmyZElRURHdWQAAJB2KHQBINHV19aCgoPT09A0bNtCdBQBA0qHYAYCkmzRp\nUosWLVavXv3582e6swAASDQUOwCQdCwWKyQkJC8vLzg4mO4sAAASDcUOAKTA0KFDu3XrtmvX\nrufPn9OdBQBAcqHYAYB0WL9+PZfLnTVrFt1BAAAkF4odAEiH9u3b+/j4XLt27cKFC3RnAQCQ\nUCh2ACA1Vq1apaam5u/vX1FRQXcWAABJhGIHAFLD1NQ0ICAgMTFx165ddGcBAJBEKHYAIE3m\nz59vbm4eFBSUnZ1NdxYAAImDYgcA0kRFRWXFihXZ2dkhISF0ZwEAkDgodgAgZXx8fDp16rRx\n48bk5GS6swAASBYUOwCQMhRFrV+/vqKiIjAwkO4sAACSBcUOAKSPq6vrkCFDjh8/fuvWLbqz\nAABIEBQ7AJBKYWFhysrKM2fOZLPZdGcBAJAUKHYAIJWsrKz8/f2fPn26c+dOurMAAEgKFDsA\nkFaLFi2ysLBYtGjRly9f6M4CACARUOwAQFqpqqqGhobm5OQEBQXRnQUAQCKg2AGAFBsxYkSP\nHj127tz54MEDurMAANAPxQ4ApNvmzZsZDIafnx+Xy6U7CwAAzVDsAEC6tW7desqUKXFxcYcP\nH6Y7CwAAzVDsAEDqLV++3MDAICAgID8/n+4sAAB0QrEDAKmno6OzcuXKzMzM3377je4sAAB0\nQrEDAFng6+vr5OT0+++/JyUl0Z0FAIA2KHYAIAsYDAbvA2RnzZpFdxYAANqg2AGAjOjcufOY\nMWMuXrwYHR1NdxYAAHqg2AGA7Fi3bp2+vr6fn19eXh7dWQAAaIBiBwCyQ09Pb9WqVRkZGUuX\nLqU7CwAADVDsAECm+Pr6urq6btq06fHjx3RnAQAQNxQ7AJApDAZj+/btDAZj8uTJbDab7jgA\nAGKFYgcAsqZNmzYzZ868f//+zp076c4CACBWKHYAIIOWL1/erFmzwMDAjIwMurMAAIgPih0A\nyCBVVdXw8PD8/Px58+bRnQUAQHxQ7ABANg0ePHjAgAEHDx7866+/6M4CACAmKHYAILM2btyo\nqqo6bdq0srIyurMAAIgDih0AyKxmzZotWbIkKSlp1apVdGcBABAHFDsAkGVz5sxxdHQMCQnB\nbe0AQB6g2AGALGMymRERERRF4bZ2ACAPUOwAQMY5ODj4+/vfv39//fr1dGcBABAtFDsAkH3L\nli2zs7NbsmTJ69ev6c4CACBCKHYAIPuUlJS2b99eUlIyadIkLpdLdxwAAFFBsQMAudCtW7fJ\nkyffuHEjIiKC7iwAAKKCYgcA8iIsLKxp06YBAQEfPnygOwsAgEig2AGAvNDU1Ny+fXteXt6v\nv/5KdxYAAJFAsQMAOdK3b9+RI0eePXs2Ojqa7iwAAMKHYgcA8mXDhg0GBgbTpk37/Pkz3VkA\nAIQMxQ4A5IuBgcGuXbuysrImTZpEdxYAACGTvmJXnpd268KfmzdsO3buVgmnltsWxJ+KPnTo\nkPiDAYC08Pb2/vnnn0+fPn3gwAG6swAACJOUFbu7O2eaG1p27/vjjFlTh/XvbmjZ+eDT7GrL\nnJo1afTo0bTEAwBpsWXLlqZNm86cOTMtLY3uLAAAQiNNxe7z30u7TNmcxdH0mbVoy/ZN8337\nkw/3xzm1inpfSHc0AJAy2traERER+fn5EyZMwC2LAUBmSFOxixizkTDU9j99c+D3lVMnTw/d\ndSbpr3BldtakbpNrfU8WAKAeHh4ekyZN+uuvv7Zv3053FgAA4ZCmYrctpUDPfsPoVjr8KSbd\n/f5a1jk/5fDQ3Uk0BgMAKRUeHm5tbR0QEJCcnEx3FgAAIZCmYlfI5igbNK020SnwnKe+ytVZ\nAxOKK2lJBQDSS01Nbd++fWVlZWPHjmWz2XTHAQD4XtJU7HpqK2c9XF3I/s+7rpSC1v6zC9ml\nrz2HbcLbsQDQUF26dJk5c+adO3fCw8PpzgIA8L2kqdgF+tqW5lx1HLn0RXpR1emGzov/9LV7\nf8HfzW9HHhvtDgAaZtWqVXZ2dkFBQU+fPqU7CwDAd5GmYtdh+YWRbXVfRS9va6Zl0rzlia8l\n/FneW2MW9reK2zilSRPr3ZlF9QwCAFCNiopKZGQkl8sdOXJkcXEx3XEAABpPmoodg2V48GHS\n7uUz3Nq3LM/JyKv89+Acg6m76nTCgRWTmylkvivFyXYA0DCOjo4rV658+fKlv78/3VkAABpP\nmoodIYTB1J8YtPHWw4QvuQXjjFT/M49S9Fm8/WVm/odXT69fPk9TQACQVnPmzPHw8NixY0dU\nVBTdWQAAGknKip0AFExbtO3h4UV3DACQMgwGIzIy0sjIaMqUKfg4CgCQUrJX7AAAGsnIyGjv\n3r25ubk+Pj64+wkASCMm3QGErDw/1sJmGCEkIyNDkOXZbPb58+dLS0vrWSYlJYUQwuFwhBEQ\nACSal5fXtGnTNm/eHBYWtnDhQrrjAAA0jKwVOy63PDMzU/Dlr1+/PnDgQEGWfPfuXWNDAYA0\nWbt2bUxMTHBwsLu7e+fOnemOAwDQALJW7BTVO969e1fw5d3d3U+fPl3/EbutW7feuHGjefPm\n350OAKSAkpLS4cOHO3bsOHr06MePH2tqatKdCABAULJW7CgFDWdnZ8GXV1BQGDBgQP3LnD9/\nnhDCYOB8RAB50apVq7Vr106bNm3SpEl//PEH3XEAAAQlrcUuJ+NdUlLyp+z8ouJSprKall6T\nFrZ2lsbadOcCABkxderUGzduREVFubm5zZgxg+44AAACkbJix2XnRf2+bGPE4bjETzXnNrF1\n+dnXL8jvJ20mJf5sACBj9uzZ8/z58zlz5jg6Orq6utIdBwDg26Sp2LHLP47v5BD57KsCS9e5\n58C2dlbG+tpKSszKsrLcL5mpyfFxMffCA0YeOHz26Z0DJop45xQAvou6unpUVJSLi8uIESMe\nPXqkr69PdyIAgG+QpmJ3Z45n5LOvbtM3HAmdaqZWS3JO+dcjYdN8gg97zPCN39FD7AEBQNa0\nadNm586do0ePHjt27JkzZ3CuLQBIOGl6kVoYmaxuPCVm08xaWx0hhKGoNyro6DZnozdHF4s5\nGwDIqlGjRo0fP/78+fOhoaF0ZwEA+AZpKnbPiyrUzb9xBSshxLGbYUVxvBjyAICc2Lp1a/v2\n7YOCgq5cuUJ3FgCA+khTsfPWU8lJDM0sr/cTIDgle6JSlHX6iCsUAMg+ZWXlqKgoDQ2N0aNH\np6en0x0HAKBO0lTsFoX1KcuLsXcZfvDSwyI2t/psbllCzAlfD7ttKfk9goPpCAgAMsva2nr3\n7t2fP38eMWJERUUF3XEAAGonTRdPtBgbvet+78lbj/t4HlNQ1LJsYWVioK2kxGKXl+V9yXib\n/Ca7tJKiKPepW05Ps6M7LADImmHDhvn7+4eHh/v5+W3dupXuOAAAtZCmYkcIw3fzVS+fk1v2\nHjl//W7iy8fJ8f8ct6MYSmZWrT3c+4z0nendyZTelAAgq1avXv3y5ctt27Y5ODhMnjyZ7jgA\nANVJV7EjhBBT50G/OQ/6jRBuZUlubkFRSbmiiqqGto4KbkoMACKmoKBw8OBBZ2fnGTNm2Nra\ndu/ene5EAAD/IU3n2FVDMVV09A3NmpoZ6uui1QGAeOjq6p4+fVpFReWnn356//493XEAAP5D\niosdAAAt7OzsDhw4kJWV5e3tXVxcTHccAIB/odgBADSYt7d3UFDQ48ePf/nlF7qzAAD8C8UO\nAKAxgoODf/zxx0OHDq1du5buLAAA/0CxAwBoDIqiIiIiWrduHRgYeO7cObrjAAAQgmIHANBo\nGhoap06d0tXVHTFixOPHj+mOAwCAYgcA8B2srKzOnTvH4XD69euXlpZGdxwAkHcodgAA36VT\np0779+//9OlT3759c3Nz6Y4DAHINxQ4A4HsNGzbst99+i4+PHzFiRGVlJd1xAEB+odgBAAjB\n/Pnzp06deunSpSlTptCdBQDkl/R9pBgAgGTauHHj+/fvIyIirK2tAwMD6Y4DAPIIR+wAAIRD\nQUHh0KFDDg4OCxcuPHr0KN1xAEAeodgBAAiNhobG2bNnTUxp9vJpAAAgAElEQVRMxo4de+nS\nJbrjAIDcQbEDABAmMzOzy5cva2hoDBky5Pbt23THAQD5gmIHACBkrVq1On/+PIPB6N+//5Mn\nT+iOAwByBMUOAED4nJycTp06VVpa2q9fv3fv3tEdBwDkBYodAIBI9OzZ8+jRo58/f/bw8MjI\nyKA7DgDIBRQ7AABRGTRo0O7du9++fdunT5+cnBy64wCA7EOxAwAQobFjx65ater58+cDBw4s\nKiqiOw4AyDgUOwAA0VqwYEFAQMDt27cHDBhQXFxMdxwAkGUodgAAIrdmzZp58+Zdv37d09MT\nx+0AQHRQ7AAAxCE0NHT69OkxMTGDBg0qKSmhOw4AyCYUOwAAcaAoauPGjdOmTbt69aq3tze6\nHQCIAoodAICYUBS1adOmqVOnXrlyZdCgQaWlpXQnAgBZg2IHACA+FEVt3rzZ19f38uXLQ4cO\nLSsrozsRAMgUFDsAALGiKGrHjh0TJkw4f/68t7c3rpMFACFCsQMAEDcGg7Fr167JkydfunSp\nT58+eXl5dCcCABmBYgcAQAMGg7Ft27Z58+bdvn27Z8+eWVlZdCcCAFmAYgcAQA+KosLCwkJD\nQx89etStW7cPHz7QnQgApB6KHQAAnebPn79169ZXr165ubm9fv2a7jgAIN1Q7AAAaPbrr7/u\n2bPnw4cP3bt3j4+PpzsOAEgxFDsAAPqNHTs2Kirq69ev3bp1u3XrFt1xAEBaodgBAEiEIUOG\nnD17trKysnfv3kePHqU7DgBIJRQ7AABJ0atXr9jYWCMjo59//nnp0qV0xwEA6YNiBwAgQezt\n7e/cudOuXbtly5b5+vpWVlbSnQgApAmKHQCAZDExMbl165aXl1dERES/fv0KCgroTgQAUgPF\nDgBA4qirq586dWrcuHGXL1/+4Ycf0tPT6U4EANIBxQ4AQBKxWKw9e/YsWbLkwYMHnTp1unfv\nHt2JAEAKoNgBAEgoiqKWLVt29OjR3Nzc7t2779mzh+5EACDpUOwAACTa8OHD4+LijI2NJ06c\nOHny5IqKCroTAYDkQrEDAJB0Dg4O9+/fd3d337lzZ69evbKysuhOBAASCsUOAEAK6OvrX7x4\ncfLkybdu3ercufOLFy/oTgQAkgjFDgBAOigqKm7fvn3btm1paWkuLi779++nOxEASBwUOwAA\naTJlypSbN2/q6emNGzduwoQJxcXFdCcCAAmCYgcAIGU6d+787NmzoUOH7t2719HREW/LAgAf\nih0AgPTR0tKKjo5ev379mzdvnJycIiIi6E4EABIBxQ4AQCpRFOXn53ft2jU9PT1fX99JkyYV\nFRXRHQoAaIZiBwAgxdzc3B4/fuzp6bl79+727dvjAyoA5ByKHQCAdNPX1z9//vyOHTs+fvzo\n5uYWGBhYXl5OdygAoAeKHQCA1KMo6pdffnnw4EHbtm3DwsLc3NySkpLoDgUANECxAwCQEXZ2\ndvfu3QsODn706FH79u03bNjA5XLpDgUAYoViBwAgO5hM5tKlS2/cuNGkSZNZs2Z5eHi8efOG\n7lAAID4odgAAssbNze3p06eTJ0++du1a27Zt161bx2az6Q4FAOKAYgcAIIM0NDS2b99+69Yt\nc3PzgICAjh07Pnz4kO5QACByKHYAADKLdzOU4ODghIQEFxeXwMDA0tJSukMBgAih2AEAyDJl\nZeWlS5feu3ePd8Gsg4PDxYsX6Q4FAKKCYgcAIPvatWt37969NWvWpKene3l5DRo06N27d3SH\nAgDhQ7EDAJALTCYzICDg1atXPj4+p0+fbt26dWBgYGFhId25AECYUOwAAOSIsbHxgQMHrl+/\nbm1tHRYW1qZNm5MnT9IdCgCEBsUOAEDudO/e/dGjR+vXr8/NzR08eHCPHj3u379PdygAEAIU\nOwAAecRkMv38/BITEydPnhwbG+vs7DxixAjczRhA2qHYAQDILyMjo+3bt7948WLYsGFRUVF2\ndnaTJ0/+9OkT3bkAoJFQ7AAA5J2NjU1UVNTNmzcdHR137txpY2OzYsWK/Px8unMBQIOh2AEA\nACGEdO3aNS4uLjo62tDQcMmSJc2bN1+1alVBQQHduQCgAVDsAADgHxRFDRs2LDExMSoqSk9P\nb/Hixc2aNVu6dGleXh7d0QBAICh2AADwHwwG48cff4yPj9+9e7eWltayZcusra1DQ0NR7wAk\nH4odAADUgsViTZw4MSkpaefOnWpqagsWLDA3N58/f356ejrd0QCgTih2AABQJxaLNWnSpOTk\n5L1795qZma1evdrS0tLX1zcxMZHuaABQCxQ7AAD4BhaLNW7cuBcvXpw+fbpTp04RERGtW7ce\nPHhwTEwM3dEA4D9Q7AAAQCAURQ0YMCAmJiY2NnbAgAGnT5/u1q1bu3btdu/eXVJSQnc6ACAE\nxQ4AABrK1dX15MmTL1++9PPzS0lJmTRpkpmZ2bx581JSUuiOBiDvUOwAAKAxWrZsuX79+g8f\nPmzZssXIyGjNmjXW1tYDBgw4depUZWUl3ekA5BSKHQAANJ66uvrUqVPj4+OvXLnSv3//Cxcu\nDBo0yNzcfMGCBcnJyXSnA5A7KHYAAPC9KIrq1avXyZMnU1NTV65cqaKiEhoaamNj06NHj4MH\nDxYVFdEdEEBeoNgBAIDQmJqaLlq06PXr11evXh0xYsS9e/d8fHyaNGni4+Nz8eJFNptNd0AA\nGYdiBwAAQkZR1A8//HD48OGPHz9u3ry5TZs2hw4d8vLyMjU1nTVr1v379+kOCCCzUOwAAEBU\ndHV1p02bFhcXl5ycvHz5cm1t7Q0bNjg5OVlbWwcGBt6/f5/L5dKdEUCmoNgBAIDIWVlZBQUF\nJSYm/v3337NmzSovLw8LC3NycmrevPmcOXPu3LmDhgcgFCh2AAAgPp06dfr9999TU1Pv3Lkz\nZ84ciqLCw8NdXV2bNm06ZcqUs2fP4l7HAN8DxQ4AAMSNoigXF5e1a9e+e/fu/v37gYGBGhoa\nO3bsGDBggL6+/sCBA3ft2pWenk53TADpw6Q7AAAAyLWOHTt27NgxJCTk9evXZ8+ePXv27MWL\nF8+cOUNRVNu2bT08PHr37t21a1dlZWW6kwJIARyxAwAAiWBtbT1r1qyrV69mZWX98ccfo0eP\nzszMXLt2be/evXV1dT09PcPDw589e4az8QDqgSN2AAAgWbS0tIYPHz58+HAul/vs2bPLly9f\nvnz55s2bly5dIoTo6+t369atR48e7u7urVu3piiK7rwAEgTFDgAAJBRFUQ4ODg4ODnPnzi0u\nLr5169a1a9du3rx56tSp48ePE0IMDAy6du3q5ubm6uraoUMHFotFd2QAmqHYAQCAFFBVVfX0\n9PT09CSE5Ofnx8TE3Lhxo2rJU1FR6dSpU5cuXVxdXV1cXPT19emODEADFDsAAJAympqa/fr1\n69evHyGkoKDg7t27cXFxcXFxd+/evXXrFm8ZS0vLTp06OTk5derUqUOHDmpqarRGBhATFDsA\nAJBiGhoaHh4eHh4ehBA2m/3ixYvY2Ni///77/v370dHRf/zxByFEQUGhVatW7du3b9euHe9f\nbW1tuoMDiIS0FrucjHdJScmfsvOLikuZympaek1a2NpZGmNHBQCQXwoKCrxz8qZOnUoIyc/P\nf/jwIa/kPXz48MCBAwcOHOAtaWlp2a5du7Zt29rb27dp08bKykpBQYHW7ADCIWXFjsvOi/p9\n2caIw3GJn2rObWLr8rOvX5DfT9pMXCQFACDvNDU13d3d3d3def/Nycl5/Pjx48ePnzx58vjx\n49OnT/NOziOEqKiotGrVyt7evlWrVnZ2dnZ2ds2bN0fVA2kkTcWOXf5xfCeHyGdfFVi6zj0H\ntrWzMtbXVlJiVpaV5X7JTE2Oj4u5Fx4w8sDhs0/vHDBRxC36AADgXzo6Oj179uzZsyfvv6Wl\npQkJCS9evHjx4sXz58/j4+P379/PX1hJScnGxsbGxsbW1tbGxqZFixbW1ta6uro0ZQcQlDQV\nuztzPCOffXWbvuFI6FQztVqSc8q/Hgmb5hN82GOGb/yOHmIPCAAAUkNZWblDhw4dOnTgT8nN\nzU1ISEhISEhKSkpISEhMTDx27BiHw+EvoKen16JFixYtWlhZWVn+n7GxMR3xAWonTcVuYWSy\nuvGUmE0z61qAoag3Kuho4fmbfkcXkx23xZkNAACknba2tqurq6urK39KaWnpq1evkpOTk5OT\nX79+nZyc/OrVq7t371Z9lIqKSvPmzZs3b25hYWFhYWFubm5ubt6sWTNjY2PcPBnET5qK3fOi\nCnXbAd9czLGbYcWDeDHkAQAA2aasrNy2bdu2bdtWnVhYWPi2infv3r19+/bq1atlZWVVF1NU\nVDQ1NTUzM2vatCnvCzMzMxMTEzMzM0NDQ0VFRfGuCsgLaSp23noqRxNDM8s9m9Rz/hynZE9U\nirKOlxhzAQCAHFFXV6/Z9gghGRkZqampaWlpaWlpqampqampHz58SEpKiomJqTmIkZGRkZGR\nmZmZkZGRiYmJoaGhoaGhiYmJgYFBkyZNdHR0xLIqIIOkqdgtCuuzf9xxe5fh60MWDO7VQU3h\nv4e4uWUJt8+HL50dkZLfd3MwTRlByjx58iQnJ6dly5ampqbfP1pFRcXHjx8JIaampkL5aKOP\nHz++evVKR0enXbt23z8aIWT16tUpKSn9+/fv27fv948m9PVdvnx5dHS0ubn5uXPnvn80QsiO\nHTvS0tJ69+7dvXv37x8tIyMjIiKCEDJx4kShnFaVmJj48OFDIyOjXr16ff9ohJDZs2e/e/du\n6NChPj4+3z/aly9foqOjCSE//vijUD7FIT8/PysrS1VVVTJPSvv+9TU2NjY2NnZxceH9l7//\n2tnZffjwIT09PS0tLSMj4+PHj5mZmbx/b9y4UVxcXHMoFoulr69vYGBgYGBgaGhoYGCgr6/P\nZDLV1NQsLS2bNm2qp6enp6enoqLyPassXBK+fYX+eiW5uNKEvWvaDwyKIoQoKGq1aN2he4+e\nvfv0+cG9R8c2NrrKTEIIRVHuU7dUCvW7jhs3jhCyYsUKoY4KdMrLy/Pw8GAy//3DRkNDY+nS\npY0e8O3btz4+Purq6rzR1NXVfXx83r592+gBly5dqqGhwY/HZDI9PDzy8vIaN1piYmLNF7Lm\nzZs3Op7Q17fqtuChKOrChQuNjqeqqlptwLZt2zY6XnBwcLWTpSiKCg4ObvSAo0aNqnYrDWtr\n68+fPzdutAcPHjAY1d/H0NbWbnS8jRs3VtsiTCZz48aNjR7w3Llzrq6u/JDm5uarVq0qLS1t\n9IDCJfT1bdD+m5+fn5iYGBMTEx0dvWnTpqCgIF9f34EDB7q6urZs2VJTU5PUTUVFxcTExN7e\nvmvXrt7e3uPHj/f391++fPnGjRv3799/6tSpmzdvPnny5O3bt9nZ2Ww2u9FrVD8J375Cf73i\ncrm3b98mhKxfv15YIYVIuoodl8vlfrh7YsHk4Q4tzZUY/77OUgylpi0cfvpl3sm/Pwj9O6LY\nyZgPHz7w9nAmk2lvb9+5c2dzc3PeD1KPHj0aMWBsbKyWlhYhpGvXrnPnzp07d66bmxshREtL\nKy4urhED8g8vmZubd+7cuXXr1rzfOurq6unp6Q0drerRL4qiqvYJJpPZiHhCX996fm+FhoY2\ndLQbN27wH66goKCoqMjvZMrKyo2I17t3b/6ALBarakX29PRsxIAWFha8h6uqqpqamvLvoKGg\noPDq1auGjsa/425NFEU1It7PP//Mf7iampqamhr/CfTx8WnEgMHBwbyV/emnnxYsWPDrr7/y\nngEXF5f8/PxGDChcQl9f4e6/eXl5Tk5OhBBjY2Nvb+9BgwY5OjryfggdHBz69evXuXNnW1tb\nQ0NDQY5Cqaurm5iY2NnZderUqVevXkOGDBkzZsy0adPmz58fGhq6cePG3bt3R0VFXbhw4ebN\nmw8ePEhOTn7//n12dnZlZZ0HTCR8+wr99YoHxU4kOBXF2Vmf3qe9/5T1tbiCI7pvhGInYywt\nLXkvOiUlJfyJjx494rW9hh6GycvLMzIy0tDQuHLlStXpV65c0dDQMDIyauhhtiVLlvBefx8/\nfsyfWFJSwnt/x8rKqkGjcf9fmxQUFDIyMvgTf/vtN970Zs2aNWg0oa8v/0hJ3759+ROfP3/O\n/1XUoNG4XC7vt7KiomJWVhZ/4pYtW3ijtWnTpkGj8Wuik5NT1em837WEkBs3bjRowBEjRvA2\nR9UnsKCgwMTEhBCioaHRoNG4VWrxu3fv+BOnT5/Om9jQ43b379/nPdDLy6vqdC+vf05cvn//\nfoMGPHPmDCHE2dm5aqepqKiYP38+IWTMmDENGk3ohL6+Qt9/ee+qz58/v6Kigj/x48ePzs7O\nhJCzZ89WXTg/Pz81NfXp06c3b948ffr0gQMHNm7cuGLFirlz5/7yyy/Dhw/39PR0dXW1t7e3\nsLDQ0dGpeaC3HiwWS0dHp2nTppaWlo6Oji4uLr169eLFMDAwGD169Lx58wIDA0NDQ8PCwvr0\n6UMI6d69e1RU1OXLl69cufL3338/ePAgISHhzZs3vLKYnZ3d0GejoYT+esUnycWO4tb75zIQ\nQsaPH79v374VK1YsXryY7izwve7evdu5c2dDQ8NPn6p/eMmzZ8/atWunrq6en58v+IC///67\nv7//vn37xo4dW23Wvn37xo8fHx4ePnv2bMEH1NTULCwsfPbsmb29fbVZRkZGnz9/vnPnDv8k\nnm9auHBhSEgIIaTmnr5u3bqAgIBaZ9VD6OvL62FDhgw5duxYrbNcXFzu3Lkj4GirV6+eP38+\nRVFV7z3Gs3v37kmTJpEGrq+WllZ+fr6xsXF6enq1WcbGxpmZmZqamnl5eYIPyGQy2Wz2jRs3\nap72p6SkVF5efvLkSW9vbwFHGzt2LO+IXc2VCggIWLduXa2z6mFiYpKRkWFra/vy5ctqs2xt\nbZOSkmp9Kurh6ur64sWL5ORkIyOjarP69+9//vz5lJQU/iFz8RP6+gp3/01LS2vWrFnfvn3P\nnj1bbVZmZmbLli3t7e3j4uIEj1dTcXFxYWFhYWFhTk5OcXFxcXFxfn5+QUFBcXFxUVFRXl5e\naWlpUVFRfn5+aWkpb8mKioqcnJyKiorCwsK8vLya+1pDqaioKCsrUxTF+wBfJSUl3qkU6urq\nvMOQWlpavA7K/0JTU5P35oOysjLvREMGg8E7Mld14rlz5/bv3z937lzecXc1NbUWLVrwTqBs\n3OsVX2xsrJub2/r16/38/L5z9YVO3osdm80+f/58aWlpPcts3br1xo0by5Yt4/0pBlJt4sSJ\ne/bsmTt37urVq2vOtbCwSEtLy8zMrPlLqC59+/a9efNmdna2kpJStVllZWW6uro9evQQ/FKA\njIwMExMTc3Pz1NTUmnPnzZu3Zs2aiRMn7t69W8ABtbW18/Ly6vrlxGtOtZaMugh3fcPCwgID\nA0kd5UNBQYHD4dTa0urCK1stWrR49epVzbkMBoPL5d67d49/vO2beA95+/Zt8+bNq8169+6d\n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\numyliwAAAABJRU5ErkJggg=="
     },
     "metadata": {
      "image/png": {
       "height": 420,
       "width": 420
      }
     },
     "output_type": "display_data"
    }
   ],
   "source": [
    "#lets plot it and see the distribuiton\n",
    "plot(train_iris$SepalWidthCm,train_iris$isVersicolor)\n",
    "#now we will be plotting the curve values\n",
    "curve(predict(glm1,data.frame(SepalWidthCm=x),type='response'),add=TRUE)\n",
    "#the graph below tell us about the values that we original and the values that are predicted."
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 15,
   "id": "894de810",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:14.857846Z",
     "iopub.status.busy": "2022-05-23T22:20:14.856283Z",
     "iopub.status.idle": "2022-05-23T22:20:14.879763Z",
     "shell.execute_reply": "2022-05-23T22:20:14.877558Z"
    },
    "papermill": {
     "duration": 0.04355,
     "end_time": "2022-05-23T22:20:14.882738",
     "exception": false,
     "start_time": "2022-05-23T22:20:14.839188",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "<strong>1:</strong> 0.842908612697772"
      ],
      "text/latex": [
       "\\textbf{1:} 0.842908612697772"
      ],
      "text/markdown": [
       "**1:** 0.842908612697772"
      ],
      "text/plain": [
       "        1 \n",
       "0.8429086 "
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#let's try to predict the probability of the of the new data\n",
    "newdata <- data.frame(SepalWidthCm=2.4)\n",
    "predict(glm1,newdata,type='response')"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "ffb973fd",
   "metadata": {
    "papermill": {
     "duration": 0.015801,
     "end_time": "2022-05-23T22:20:14.914133",
     "exception": false,
     "start_time": "2022-05-23T22:20:14.898332",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "The point here is to note is that, we have only used one variable aganist another variable, but you can take all the variable and build the model out of it."
   ]
  },
  {
   "cell_type": "markdown",
   "id": "7ca032e2",
   "metadata": {
    "papermill": {
     "duration": 0.015685,
     "end_time": "2022-05-23T22:20:14.946449",
     "exception": false,
     "start_time": "2022-05-23T22:20:14.930764",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "> > Classification Trees"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 16,
   "id": "afeb0417",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:14.981188Z",
     "iopub.status.busy": "2022-05-23T22:20:14.979454Z",
     "iopub.status.idle": "2022-05-23T22:20:15.007834Z",
     "shell.execute_reply": "2022-05-23T22:20:15.004690Z"
    },
    "papermill": {
     "duration": 0.050289,
     "end_time": "2022-05-23T22:20:15.012363",
     "exception": false,
     "start_time": "2022-05-23T22:20:14.962074",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "'data.frame':\t90 obs. of  7 variables:\n",
      " $ Id           : int  142 55 66 39 60 2 134 67 44 103 ...\n",
      " $ SepalLengthCm: num  6.9 6.5 6.7 4.4 5.2 4.9 6.3 5.6 5 7.1 ...\n",
      " $ SepalWidthCm : num  3.1 2.8 3.1 3 2.7 3 2.8 3 3.5 3 ...\n",
      " $ PetalLengthCm: num  5.1 4.6 4.4 1.3 3.9 1.4 5.1 4.5 1.6 5.9 ...\n",
      " $ PetalWidthCm : num  2.3 1.5 1.4 0.2 1.4 0.2 1.5 1.5 0.6 2.1 ...\n",
      " $ Species      : chr  \"Iris-virginica\" \"Iris-versicolor\" \"Iris-versicolor\" \"Iris-setosa\" ...\n",
      " $ isVersicolor : logi  FALSE TRUE TRUE FALSE TRUE FALSE ...\n"
     ]
    }
   ],
   "source": [
    "str(train_iris)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 17,
   "id": "2c070112",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:15.052813Z",
     "iopub.status.busy": "2022-05-23T22:20:15.051138Z",
     "iopub.status.idle": "2022-05-23T22:20:15.076716Z",
     "shell.execute_reply": "2022-05-23T22:20:15.074647Z"
    },
    "papermill": {
     "duration": 0.050601,
     "end_time": "2022-05-23T22:20:15.079535",
     "exception": false,
     "start_time": "2022-05-23T22:20:15.028934",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "'data.frame':\t90 obs. of  5 variables:\n",
      " $ SepalLengthCm: num  6.9 6.5 6.7 4.4 5.2 4.9 6.3 5.6 5 7.1 ...\n",
      " $ SepalWidthCm : num  3.1 2.8 3.1 3 2.7 3 2.8 3 3.5 3 ...\n",
      " $ PetalLengthCm: num  5.1 4.6 4.4 1.3 3.9 1.4 5.1 4.5 1.6 5.9 ...\n",
      " $ PetalWidthCm : num  2.3 1.5 1.4 0.2 1.4 0.2 1.5 1.5 0.6 2.1 ...\n",
      " $ Species      : chr  \"Iris-virginica\" \"Iris-versicolor\" \"Iris-versicolor\" \"Iris-setosa\" ...\n"
     ]
    }
   ],
   "source": [
    "tree_data <- train_iris[,2:6]\n",
    "str(tree_data)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 18,
   "id": "506f041c",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:15.114800Z",
     "iopub.status.busy": "2022-05-23T22:20:15.113142Z",
     "iopub.status.idle": "2022-05-23T22:20:15.128372Z",
     "shell.execute_reply": "2022-05-23T22:20:15.126410Z"
    },
    "papermill": {
     "duration": 0.035771,
     "end_time": "2022-05-23T22:20:15.131362",
     "exception": false,
     "start_time": "2022-05-23T22:20:15.095591",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "tree_data[, 'Species'] <- as.factor(tree_data[, 'Species'])"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 19,
   "id": "7db0d5a4",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:15.166043Z",
     "iopub.status.busy": "2022-05-23T22:20:15.164298Z",
     "iopub.status.idle": "2022-05-23T22:20:15.189336Z",
     "shell.execute_reply": "2022-05-23T22:20:15.187389Z"
    },
    "papermill": {
     "duration": 0.044737,
     "end_time": "2022-05-23T22:20:15.192123",
     "exception": false,
     "start_time": "2022-05-23T22:20:15.147386",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "'data.frame':\t90 obs. of  5 variables:\n",
      " $ SepalLengthCm: num  6.9 6.5 6.7 4.4 5.2 4.9 6.3 5.6 5 7.1 ...\n",
      " $ SepalWidthCm : num  3.1 2.8 3.1 3 2.7 3 2.8 3 3.5 3 ...\n",
      " $ PetalLengthCm: num  5.1 4.6 4.4 1.3 3.9 1.4 5.1 4.5 1.6 5.9 ...\n",
      " $ PetalWidthCm : num  2.3 1.5 1.4 0.2 1.4 0.2 1.5 1.5 0.6 2.1 ...\n",
      " $ Species      : Factor w/ 3 levels \"Iris-setosa\",..: 3 2 2 1 2 1 3 2 1 3 ...\n"
     ]
    }
   ],
   "source": [
    "str(tree_data)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 20,
   "id": "7f9e35e0",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:15.227012Z",
     "iopub.status.busy": "2022-05-23T22:20:15.225301Z",
     "iopub.status.idle": "2022-05-23T22:20:15.269996Z",
     "shell.execute_reply": "2022-05-23T22:20:15.268182Z"
    },
    "papermill": {
     "duration": 0.064901,
     "end_time": "2022-05-23T22:20:15.272882",
     "exception": false,
     "start_time": "2022-05-23T22:20:15.207981",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "library(tree)#loading the library\n",
    "tree1 <- tree(Species~.,data = tree_data)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 21,
   "id": "799c0638",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:15.308640Z",
     "iopub.status.busy": "2022-05-23T22:20:15.306921Z",
     "iopub.status.idle": "2022-05-23T22:20:15.399847Z",
     "shell.execute_reply": "2022-05-23T22:20:15.396768Z"
    },
    "papermill": {
     "duration": 0.113233,
     "end_time": "2022-05-23T22:20:15.402732",
     "exception": false,
     "start_time": "2022-05-23T22:20:15.289499",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "image/png": "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\nAAAUQdgBAAAogrADAABQBGEHAACgCMIOAABAEYQdAACAIgg7AAAARRB2AAAAiiDsAAAAFEHY\nAQAAKIKwAwAAUARhBwAAoAjCDgAAQBGEHQAAgCIIOwAAAEUQdgAAAIog7AAAABRB2AEAACiC\nsAMAAFAEYQcAAKAIwg4AAEARhB0AAIAiCDsAAABFEHYAAACKIOwAAAAUQdgBAAAogrADAABQ\nBGEHAACgCMIOAABAEYQdAACAIgg7AAAARRB2AAAAiiDsAAAAFEHYAQAAKIKwAwAAUARhBwAA\noAjCDgAAQBGEHQAAgCIIOwAAAEUQdgAAAIog7AAAABRB2AEAACiCsAMAAFAEYQcAAKAIwg4A\nAEARhB0AAIAiCDsAAABFEHYAAACKIOwAAAAUQdgBAAAogrADAABQBGEHAACgCMIOAABAEYQd\nAACAIgg7AAAARRB2AAAAiiDsAAAAFEHYAQAAKIKwAwAAUARhBwAAoAjCDgAAQBGEHQAAgCII\nOwAAAEUQdgAAAIog7AAAABRB2AEAACiCsAMAAFAEYQcAAKAIwg4AAEARhB0AAIAiCDsAAABF\nEHYAAACKIOwAAAAUQdgBAAAogrADAABQBGEHAACgCMIOAABAEYQdAACAIgg7AAAARRB2AAAA\niiDsAAAAFEHYAQAAKIKwAwAAUARhBwAAoAjCDgAAQBGEHQAAgCIIOwAAAEUQdgAAAIog7AAA\nABRB2AEAACiCsAMAAFAEYQcAAKAIwg4AAEARhB0AAIAiCDsAAABFEHYAAACKIOwAAAAUQdgB\nAAAogrADAABQBGEHAACgCMIOAABAEYQdAACAIgg7AAAARRB2AAAAiiDsAAAAFEHYAQAAKIKw\nAwAAUARhBwAAoAjCDgAAQBGEHQAAgCIIOwAAAEUQdgAAAIog7AAAABRB2AEAACiCsAMAAFAE\nYQcAAKAIwg4AAEARhB0AAIAiCDsAAABFEHYAAACKIOwAAAAUQdgBAAAogrADAABQBGEHAACg\nCMIOAABAEYQdAACAIgg7AAAARRB2AAAAiiDsAAAAFEHYAQAAKIKwAwAAUARhBwAAoAjCDgAA\nQBGEHQAAgCIIOwAAAEUQdgAAAIog7AAAABRB2AEAACiCsAMAAFAEYQcAAKAIwg4AAEARhB0A\nAIAiCDsAAABFEHYAAACKIOwAAAAUQdgBAAAogrADAABQBGEHAACgCMIOAABAEYQdAACAIgg7\n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\nz5z3TAtfm/XS2LFj83q6+w/ODXbedSa18Ji5P26iTbYbRGa+f9jJK2JK/UolrCE7/bCIuNV2\nu6GHZgp2Mh12uCOVzSX6njPz0gkR8WzoWVQHa9rhyS+98sE/N13KytEMZv+gOve3ihRJLMng\nxeKrWAAAUIYMmqOtmsln4IS5CUfOXT516MuVC0c81+nY1s/6tGmy7WqW0Sk4/9lj184uvP4c\nc2z34MwrO+b8lpKWtGJlUlqDobEmh7Pkr8FoqSYiqcdTb+xi+y2rdH43wuzpIyJpJ9OK6jAh\n/MFpS7/qMCJ2+/+OpmRmnkk8uPYf75XK1EKwAwAA5SUjec24cePeW3VCRLxqNOjca/B7H8d/\n81aznKykmAMXHTyx+ZTnRWTxzP2H5szSNMPbo5uUfFLXqs85G7Rzm1fmb0w9uziplIKdu/9g\nTdMSP9mQvzHr2k6jwVC16Qpr2oGZPyZ7h8xa9c6ItveFuJo0EbFlny+VqYVgBwAAyo8eExMz\nafjEZGvePd70Xfsuikion4uDp3kGvRbh5ZT4j2lT/3bEq9brj/s4l3xKo1PgwqjAlLMfDVv6\nQ26LLTtpUo/iL54oIYtX+zeb+Fw8+PqE+GPX2/TPRw6y6foDE8NFMxk0zZr2i/X6zfps2eff\nH/akiIiUQrLkHDsAAFA+nCt3m96h+vjNy4OC90dFNPdzsx3auX7z/t/92ox6u5aXw6caZvQN\naTP/yzUiXRYVefu6ovT6bP2K0PAPB7TcvfTx+4Nc9mxee9yzb6jbokSTxy3vS35jvl72WZ1u\nM7rW3xgR1byh/6m9Gzck/ObTZODKJ2uZjNq0tn7jti+o1/7i05GN038/tj3u32eCngh0OnTu\nxJsz5iSPe/Wmdyc/VuwAAEC5eX3j/+aPG1TP7cL6zz9ZsOyzE1rt4VM/Pri1mHPmRCR0/DAR\nMZp950cH3uykJtdGXx7c/0b/LilHvln2701V2r+6N2He6awco8XOLZRvgUvV6F0/f/Na3+iL\nh75dsnD5nnPe/V6bfWDPInejJiKvfZ0wdUhXOfKf2e/O3fLT2Qf/uvTEdyuXjH7CzXZkRszf\nbnNqTdcd/G4HAKVERkZu3bo1Li6uS5cu5V0LKpz4+PgnnngiIiJiy5Yt5V0LKrp93+3MNFQO\na10vr8Watt/sFlqjw7pTm+z8ksQ9hBU7AABQsax4Jqpt2wd+SMnOa9n74csiEjn5/vIrqnRw\n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"
     },
     "metadata": {
      "image/png": {
       "height": 420,
       "width": 420
      }
     },
     "output_type": "display_data"
    }
   ],
   "source": [
    "plot(tree1)\n",
    "text(tree1)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 22,
   "id": "ab0e3c65",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:15.439666Z",
     "iopub.status.busy": "2022-05-23T22:20:15.437960Z",
     "iopub.status.idle": "2022-05-23T22:20:15.457802Z",
     "shell.execute_reply": "2022-05-23T22:20:15.455902Z"
    },
    "papermill": {
     "duration": 0.040994,
     "end_time": "2022-05-23T22:20:15.460702",
     "exception": false,
     "start_time": "2022-05-23T22:20:15.419708",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "node), split, n, deviance, yval, (yprob)\n",
       "      * denotes terminal node\n",
       "\n",
       " 1) root 90 197.500 Iris-setosa ( 0.3444 0.3444 0.3111 )  \n",
       "   2) PetalLengthCm < 2.6 31   0.000 Iris-setosa ( 1.0000 0.0000 0.0000 ) *\n",
       "   3) PetalLengthCm > 2.6 59  81.640 Iris-versicolor ( 0.0000 0.5254 0.4746 )  \n",
       "     6) PetalLengthCm < 5.05 35  24.880 Iris-versicolor ( 0.0000 0.8857 0.1143 )  \n",
       "      12) PetalWidthCm < 1.65 30   0.000 Iris-versicolor ( 0.0000 1.0000 0.0000 ) *\n",
       "      13) PetalWidthCm > 1.65 5   5.004 Iris-virginica ( 0.0000 0.2000 0.8000 ) *\n",
       "     7) PetalLengthCm > 5.05 24   0.000 Iris-virginica ( 0.0000 0.0000 1.0000 ) *"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#printing out the results from the object\n",
    "tree1"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 23,
   "id": "ccea70ca",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:15.496307Z",
     "iopub.status.busy": "2022-05-23T22:20:15.494546Z",
     "iopub.status.idle": "2022-05-23T22:20:15.520523Z",
     "shell.execute_reply": "2022-05-23T22:20:15.518414Z"
    },
    "papermill": {
     "duration": 0.046875,
     "end_time": "2022-05-23T22:20:15.523733",
     "exception": false,
     "start_time": "2022-05-23T22:20:15.476858",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "\n",
       "Classification tree:\n",
       "tree(formula = Species ~ ., data = tree_data)\n",
       "Variables actually used in tree construction:\n",
       "[1] \"PetalLengthCm\" \"PetalWidthCm\" \n",
       "Number of terminal nodes:  4 \n",
       "Residual mean deviance:  0.05819 = 5.004 / 86 \n",
       "Misclassification error rate: 0.01111 = 1 / 90 "
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#finding out the summary of the tree\n",
    "summary(tree1)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 24,
   "id": "894aa576",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:15.560481Z",
     "iopub.status.busy": "2022-05-23T22:20:15.558859Z",
     "iopub.status.idle": "2022-05-23T22:20:15.576654Z",
     "shell.execute_reply": "2022-05-23T22:20:15.574549Z"
    },
    "papermill": {
     "duration": 0.039179,
     "end_time": "2022-05-23T22:20:15.579658",
     "exception": false,
     "start_time": "2022-05-23T22:20:15.540479",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "#as we can see there is very low misclassification erroe in our tree model\n",
    "prediction <- predict(tree1,newdata = test_iris,type = 'class')"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 25,
   "id": "53c1b9a1",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:15.616179Z",
     "iopub.status.busy": "2022-05-23T22:20:15.614546Z",
     "iopub.status.idle": "2022-05-23T22:20:15.636573Z",
     "shell.execute_reply": "2022-05-23T22:20:15.634354Z"
    },
    "papermill": {
     "duration": 0.042973,
     "end_time": "2022-05-23T22:20:15.639357",
     "exception": false,
     "start_time": "2022-05-23T22:20:15.596384",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "<style>\n",
       ".list-inline {list-style: none; margin:0; padding: 0}\n",
       ".list-inline>li {display: inline-block}\n",
       ".list-inline>li:not(:last-child)::after {content: \"\\00b7\"; padding: 0 .5ex}\n",
       "</style>\n",
       "<ol class=list-inline><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-virginica</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-virginica</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-versicolor</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li></ol>\n",
       "\n",
       "<details>\n",
       "\t<summary style=display:list-item;cursor:pointer>\n",
       "\t\t<strong>Levels</strong>:\n",
       "\t</summary>\n",
       "\t<style>\n",
       "\t.list-inline {list-style: none; margin:0; padding: 0}\n",
       "\t.list-inline>li {display: inline-block}\n",
       "\t.list-inline>li:not(:last-child)::after {content: \"\\00b7\"; padding: 0 .5ex}\n",
       "\t</style>\n",
       "\t<ol class=list-inline><li>'Iris-setosa'</li><li>'Iris-versicolor'</li><li>'Iris-virginica'</li></ol>\n",
       "</details>"
      ],
      "text/latex": [
       "\\begin{enumerate*}\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-virginica\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-virginica\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\end{enumerate*}\n",
       "\n",
       "\\emph{Levels}: \\begin{enumerate*}\n",
       "\\item 'Iris-setosa'\n",
       "\\item 'Iris-versicolor'\n",
       "\\item 'Iris-virginica'\n",
       "\\end{enumerate*}\n"
      ],
      "text/markdown": [
       "1. Iris-setosa\n",
       "2. Iris-setosa\n",
       "3. Iris-setosa\n",
       "4. Iris-setosa\n",
       "5. Iris-setosa\n",
       "6. Iris-setosa\n",
       "7. Iris-setosa\n",
       "8. Iris-setosa\n",
       "9. Iris-setosa\n",
       "10. Iris-setosa\n",
       "11. Iris-setosa\n",
       "12. Iris-setosa\n",
       "13. Iris-setosa\n",
       "14. Iris-setosa\n",
       "15. Iris-setosa\n",
       "16. Iris-setosa\n",
       "17. Iris-setosa\n",
       "18. Iris-setosa\n",
       "19. Iris-setosa\n",
       "20. Iris-versicolor\n",
       "21. Iris-versicolor\n",
       "22. Iris-versicolor\n",
       "23. Iris-versicolor\n",
       "24. Iris-versicolor\n",
       "25. Iris-virginica\n",
       "26. Iris-versicolor\n",
       "27. Iris-versicolor\n",
       "28. Iris-versicolor\n",
       "29. Iris-versicolor\n",
       "30. Iris-versicolor\n",
       "31. Iris-versicolor\n",
       "32. Iris-virginica\n",
       "33. Iris-versicolor\n",
       "34. Iris-versicolor\n",
       "35. Iris-versicolor\n",
       "36. Iris-versicolor\n",
       "37. Iris-versicolor\n",
       "38. Iris-versicolor\n",
       "39. Iris-virginica\n",
       "40. Iris-virginica\n",
       "41. Iris-virginica\n",
       "42. Iris-virginica\n",
       "43. Iris-virginica\n",
       "44. Iris-virginica\n",
       "45. Iris-virginica\n",
       "46. Iris-virginica\n",
       "47. Iris-virginica\n",
       "48. Iris-virginica\n",
       "49. Iris-versicolor\n",
       "50. Iris-virginica\n",
       "51. Iris-virginica\n",
       "52. Iris-virginica\n",
       "53. Iris-virginica\n",
       "54. Iris-virginica\n",
       "55. Iris-virginica\n",
       "56. Iris-virginica\n",
       "57. Iris-virginica\n",
       "58. Iris-virginica\n",
       "59. Iris-virginica\n",
       "60. Iris-virginica\n",
       "\n",
       "\n",
       "\n",
       "**Levels**: 1. 'Iris-setosa'\n",
       "2. 'Iris-versicolor'\n",
       "3. 'Iris-virginica'\n",
       "\n",
       "\n"
      ],
      "text/plain": [
       " [1] Iris-setosa     Iris-setosa     Iris-setosa     Iris-setosa    \n",
       " [5] Iris-setosa     Iris-setosa     Iris-setosa     Iris-setosa    \n",
       " [9] Iris-setosa     Iris-setosa     Iris-setosa     Iris-setosa    \n",
       "[13] Iris-setosa     Iris-setosa     Iris-setosa     Iris-setosa    \n",
       "[17] Iris-setosa     Iris-setosa     Iris-setosa     Iris-versicolor\n",
       "[21] Iris-versicolor Iris-versicolor Iris-versicolor Iris-versicolor\n",
       "[25] Iris-virginica  Iris-versicolor Iris-versicolor Iris-versicolor\n",
       "[29] Iris-versicolor Iris-versicolor Iris-versicolor Iris-virginica \n",
       "[33] Iris-versicolor Iris-versicolor Iris-versicolor Iris-versicolor\n",
       "[37] Iris-versicolor Iris-versicolor Iris-virginica  Iris-virginica \n",
       "[41] Iris-virginica  Iris-virginica  Iris-virginica  Iris-virginica \n",
       "[45] Iris-virginica  Iris-virginica  Iris-virginica  Iris-virginica \n",
       "[49] Iris-versicolor Iris-virginica  Iris-virginica  Iris-virginica \n",
       "[53] Iris-virginica  Iris-virginica  Iris-virginica  Iris-virginica \n",
       "[57] Iris-virginica  Iris-virginica  Iris-virginica  Iris-virginica \n",
       "Levels: Iris-setosa Iris-versicolor Iris-virginica"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#finding out the prediction\n",
    "prediction"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 26,
   "id": "110733ef",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:15.771557Z",
     "iopub.status.busy": "2022-05-23T22:20:15.674863Z",
     "iopub.status.idle": "2022-05-23T22:20:15.788671Z",
     "shell.execute_reply": "2022-05-23T22:20:15.786968Z"
    },
    "papermill": {
     "duration": 0.134918,
     "end_time": "2022-05-23T22:20:15.791314",
     "exception": false,
     "start_time": "2022-05-23T22:20:15.656396",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "                 \n",
       "prediction        Iris-setosa Iris-versicolor Iris-virginica\n",
       "  Iris-setosa              19               0              0\n",
       "  Iris-versicolor           0              17              1\n",
       "  Iris-virginica            0               2             21"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#finding out the accuracy of the model\n",
    "table(prediction,test_iris$Species)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 27,
   "id": "a30b06a4",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:15.828531Z",
     "iopub.status.busy": "2022-05-23T22:20:15.826996Z",
     "iopub.status.idle": "2022-05-23T22:20:15.845552Z",
     "shell.execute_reply": "2022-05-23T22:20:15.843702Z"
    },
    "papermill": {
     "duration": 0.039758,
     "end_time": "2022-05-23T22:20:15.848312",
     "exception": false,
     "start_time": "2022-05-23T22:20:15.808554",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "0.95"
      ],
      "text/latex": [
       "0.95"
      ],
      "text/markdown": [
       "0.95"
      ],
      "text/plain": [
       "[1] 0.95"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#so lets check out the accuracy of the model\n",
    "(20+22+15)/60 #=0.95%, it means that we have predicted the values by the margin of 95% accuracy"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "538d66c9",
   "metadata": {
    "papermill": {
     "duration": 0.017308,
     "end_time": "2022-05-23T22:20:15.882725",
     "exception": false,
     "start_time": "2022-05-23T22:20:15.865417",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "> > Naive Bayes classifier"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 28,
   "id": "4bb06c3d",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:15.921336Z",
     "iopub.status.busy": "2022-05-23T22:20:15.919642Z",
     "iopub.status.idle": "2022-05-23T22:20:15.993729Z",
     "shell.execute_reply": "2022-05-23T22:20:15.991695Z"
    },
    "papermill": {
     "duration": 0.096523,
     "end_time": "2022-05-23T22:20:15.996798",
     "exception": false,
     "start_time": "2022-05-23T22:20:15.900275",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "#now we are going to use the naive bayes in it\n",
    "#loading the package \n",
    "library(e1071)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 29,
   "id": "241ab8af",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:16.035364Z",
     "iopub.status.busy": "2022-05-23T22:20:16.033535Z",
     "iopub.status.idle": "2022-05-23T22:20:16.060617Z",
     "shell.execute_reply": "2022-05-23T22:20:16.058634Z"
    },
    "papermill": {
     "duration": 0.048899,
     "end_time": "2022-05-23T22:20:16.063783",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.014884",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "\n",
       "Naive Bayes Classifier for Discrete Predictors\n",
       "\n",
       "Call:\n",
       "naiveBayes.default(x = X, y = Y, laplace = laplace)\n",
       "\n",
       "A-priori probabilities:\n",
       "Y\n",
       "    Iris-setosa Iris-versicolor  Iris-virginica \n",
       "      0.3444444       0.3444444       0.3111111 \n",
       "\n",
       "Conditional probabilities:\n",
       "                 Id\n",
       "Y                      [,1]     [,2]\n",
       "  Iris-setosa      23.03226 14.83124\n",
       "  Iris-versicolor  74.03226 14.47638\n",
       "  Iris-virginica  125.89286 13.59559\n",
       "\n",
       "                 SepalLengthCm\n",
       "Y                     [,1]      [,2]\n",
       "  Iris-setosa     5.058065 0.3575604\n",
       "  Iris-versicolor 5.964516 0.5332594\n",
       "  Iris-virginica  6.646429 0.6088740\n",
       "\n",
       "                 SepalWidthCm\n",
       "Y                     [,1]      [,2]\n",
       "  Iris-setosa     3.461290 0.3684901\n",
       "  Iris-versicolor 2.764516 0.3209864\n",
       "  Iris-virginica  2.971429 0.2813422\n",
       "\n",
       "                 PetalLengthCm\n",
       "Y                     [,1]      [,2]\n",
       "  Iris-setosa     1.493548 0.1547805\n",
       "  Iris-versicolor 4.306452 0.3855174\n",
       "  Iris-virginica  5.578571 0.5244801\n",
       "\n",
       "                 PetalWidthCm\n",
       "Y                      [,1]      [,2]\n",
       "  Iris-setosa     0.2612903 0.1145351\n",
       "  Iris-versicolor 1.3354839 0.1835727\n",
       "  Iris-virginica  1.9928571 0.2787918\n"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#running the first naive bayes model \n",
    "nb1 <- naiveBayes(Species ~.,data = train_iris[,-7])\n",
    "nb1"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 30,
   "id": "2fe10425",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:16.103270Z",
     "iopub.status.busy": "2022-05-23T22:20:16.101348Z",
     "iopub.status.idle": "2022-05-23T22:20:16.129500Z",
     "shell.execute_reply": "2022-05-23T22:20:16.125202Z"
    },
    "papermill": {
     "duration": 0.051143,
     "end_time": "2022-05-23T22:20:16.132721",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.081578",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "'data.frame':\t60 obs. of  6 variables:\n",
      " $ Id           : int  4 11 13 14 20 22 23 25 27 28 ...\n",
      " $ SepalLengthCm: num  4.6 5.4 4.8 4.3 5.1 5.1 4.6 4.8 5 5.2 ...\n",
      " $ SepalWidthCm : num  3.1 3.7 3 3 3.8 3.7 3.6 3.4 3.4 3.5 ...\n",
      " $ PetalLengthCm: num  1.5 1.5 1.4 1.1 1.5 1.5 1 1.9 1.6 1.5 ...\n",
      " $ PetalWidthCm : num  0.2 0.2 0.1 0.1 0.3 0.4 0.2 0.2 0.4 0.2 ...\n",
      " $ Species      : chr  \"Iris-setosa\" \"Iris-setosa\" \"Iris-setosa\" \"Iris-setosa\" ...\n"
     ]
    }
   ],
   "source": [
    "str(test_iris)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 31,
   "id": "6cff4a0e",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:16.192674Z",
     "iopub.status.busy": "2022-05-23T22:20:16.190719Z",
     "iopub.status.idle": "2022-05-23T22:20:16.218502Z",
     "shell.execute_reply": "2022-05-23T22:20:16.215324Z"
    },
    "papermill": {
     "duration": 0.071105,
     "end_time": "2022-05-23T22:20:16.222499",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.151394",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "'data.frame':\t90 obs. of  7 variables:\n",
      " $ Id           : int  142 55 66 39 60 2 134 67 44 103 ...\n",
      " $ SepalLengthCm: num  6.9 6.5 6.7 4.4 5.2 4.9 6.3 5.6 5 7.1 ...\n",
      " $ SepalWidthCm : num  3.1 2.8 3.1 3 2.7 3 2.8 3 3.5 3 ...\n",
      " $ PetalLengthCm: num  5.1 4.6 4.4 1.3 3.9 1.4 5.1 4.5 1.6 5.9 ...\n",
      " $ PetalWidthCm : num  2.3 1.5 1.4 0.2 1.4 0.2 1.5 1.5 0.6 2.1 ...\n",
      " $ Species      : chr  \"Iris-virginica\" \"Iris-versicolor\" \"Iris-versicolor\" \"Iris-setosa\" ...\n",
      " $ isVersicolor : logi  FALSE TRUE TRUE FALSE TRUE FALSE ...\n"
     ]
    }
   ],
   "source": [
    "str(train_iris)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 32,
   "id": "ded677a0",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:16.262102Z",
     "iopub.status.busy": "2022-05-23T22:20:16.260250Z",
     "iopub.status.idle": "2022-05-23T22:20:16.298260Z",
     "shell.execute_reply": "2022-05-23T22:20:16.296090Z"
    },
    "papermill": {
     "duration": 0.06088,
     "end_time": "2022-05-23T22:20:16.301428",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.240548",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "<style>\n",
       ".list-inline {list-style: none; margin:0; padding: 0}\n",
       ".list-inline>li {display: inline-block}\n",
       ".list-inline>li:not(:last-child)::after {content: \"\\00b7\"; padding: 0 .5ex}\n",
       "</style>\n",
       "<ol class=list-inline><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-setosa</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-versicolor</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li><li>Iris-virginica</li></ol>\n",
       "\n",
       "<details>\n",
       "\t<summary style=display:list-item;cursor:pointer>\n",
       "\t\t<strong>Levels</strong>:\n",
       "\t</summary>\n",
       "\t<style>\n",
       "\t.list-inline {list-style: none; margin:0; padding: 0}\n",
       "\t.list-inline>li {display: inline-block}\n",
       "\t.list-inline>li:not(:last-child)::after {content: \"\\00b7\"; padding: 0 .5ex}\n",
       "\t</style>\n",
       "\t<ol class=list-inline><li>'Iris-setosa'</li><li>'Iris-versicolor'</li><li>'Iris-virginica'</li></ol>\n",
       "</details>"
      ],
      "text/latex": [
       "\\begin{enumerate*}\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-setosa\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-versicolor\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\item Iris-virginica\n",
       "\\end{enumerate*}\n",
       "\n",
       "\\emph{Levels}: \\begin{enumerate*}\n",
       "\\item 'Iris-setosa'\n",
       "\\item 'Iris-versicolor'\n",
       "\\item 'Iris-virginica'\n",
       "\\end{enumerate*}\n"
      ],
      "text/markdown": [
       "1. Iris-setosa\n",
       "2. Iris-setosa\n",
       "3. Iris-setosa\n",
       "4. Iris-setosa\n",
       "5. Iris-setosa\n",
       "6. Iris-setosa\n",
       "7. Iris-setosa\n",
       "8. Iris-setosa\n",
       "9. Iris-setosa\n",
       "10. Iris-setosa\n",
       "11. Iris-setosa\n",
       "12. Iris-setosa\n",
       "13. Iris-setosa\n",
       "14. Iris-setosa\n",
       "15. Iris-setosa\n",
       "16. Iris-setosa\n",
       "17. Iris-setosa\n",
       "18. Iris-setosa\n",
       "19. Iris-setosa\n",
       "20. Iris-versicolor\n",
       "21. Iris-versicolor\n",
       "22. Iris-versicolor\n",
       "23. Iris-versicolor\n",
       "24. Iris-versicolor\n",
       "25. Iris-versicolor\n",
       "26. Iris-versicolor\n",
       "27. Iris-versicolor\n",
       "28. Iris-versicolor\n",
       "29. Iris-versicolor\n",
       "30. Iris-versicolor\n",
       "31. Iris-versicolor\n",
       "32. Iris-versicolor\n",
       "33. Iris-versicolor\n",
       "34. Iris-versicolor\n",
       "35. Iris-versicolor\n",
       "36. Iris-versicolor\n",
       "37. Iris-versicolor\n",
       "38. Iris-versicolor\n",
       "39. Iris-virginica\n",
       "40. Iris-virginica\n",
       "41. Iris-virginica\n",
       "42. Iris-virginica\n",
       "43. Iris-virginica\n",
       "44. Iris-virginica\n",
       "45. Iris-virginica\n",
       "46. Iris-virginica\n",
       "47. Iris-virginica\n",
       "48. Iris-virginica\n",
       "49. Iris-virginica\n",
       "50. Iris-virginica\n",
       "51. Iris-virginica\n",
       "52. Iris-virginica\n",
       "53. Iris-virginica\n",
       "54. Iris-virginica\n",
       "55. Iris-virginica\n",
       "56. Iris-virginica\n",
       "57. Iris-virginica\n",
       "58. Iris-virginica\n",
       "59. Iris-virginica\n",
       "60. Iris-virginica\n",
       "\n",
       "\n",
       "\n",
       "**Levels**: 1. 'Iris-setosa'\n",
       "2. 'Iris-versicolor'\n",
       "3. 'Iris-virginica'\n",
       "\n",
       "\n"
      ],
      "text/plain": [
       " [1] Iris-setosa     Iris-setosa     Iris-setosa     Iris-setosa    \n",
       " [5] Iris-setosa     Iris-setosa     Iris-setosa     Iris-setosa    \n",
       " [9] Iris-setosa     Iris-setosa     Iris-setosa     Iris-setosa    \n",
       "[13] Iris-setosa     Iris-setosa     Iris-setosa     Iris-setosa    \n",
       "[17] Iris-setosa     Iris-setosa     Iris-setosa     Iris-versicolor\n",
       "[21] Iris-versicolor Iris-versicolor Iris-versicolor Iris-versicolor\n",
       "[25] Iris-versicolor Iris-versicolor Iris-versicolor Iris-versicolor\n",
       "[29] Iris-versicolor Iris-versicolor Iris-versicolor Iris-versicolor\n",
       "[33] Iris-versicolor Iris-versicolor Iris-versicolor Iris-versicolor\n",
       "[37] Iris-versicolor Iris-versicolor Iris-virginica  Iris-virginica \n",
       "[41] Iris-virginica  Iris-virginica  Iris-virginica  Iris-virginica \n",
       "[45] Iris-virginica  Iris-virginica  Iris-virginica  Iris-virginica \n",
       "[49] Iris-virginica  Iris-virginica  Iris-virginica  Iris-virginica \n",
       "[53] Iris-virginica  Iris-virginica  Iris-virginica  Iris-virginica \n",
       "[57] Iris-virginica  Iris-virginica  Iris-virginica  Iris-virginica \n",
       "Levels: Iris-setosa Iris-versicolor Iris-virginica"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#now we will use our model to predict it on the test set\n",
    "prediction <- predict(nb1,test_iris[-6])\n",
    "prediction"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 33,
   "id": "76a7d19f",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:16.341535Z",
     "iopub.status.busy": "2022-05-23T22:20:16.339606Z",
     "iopub.status.idle": "2022-05-23T22:20:16.364420Z",
     "shell.execute_reply": "2022-05-23T22:20:16.362069Z"
    },
    "papermill": {
     "duration": 0.047506,
     "end_time": "2022-05-23T22:20:16.367622",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.320116",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "                 \n",
       "prediction        Iris-setosa Iris-versicolor Iris-virginica\n",
       "  Iris-setosa              19               0              0\n",
       "  Iris-versicolor           0              19              0\n",
       "  Iris-virginica            0               0             22"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#now we will be creating the table out of it\n",
    "table1 <- table(prediction,test_iris$Species)\n",
    "table1"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "481fa32a",
   "metadata": {
    "papermill": {
     "duration": 0.018488,
     "end_time": "2022-05-23T22:20:16.405008",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.386520",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "It's very surprising to see that we have receive 100% accuracy in it."
   ]
  },
  {
   "cell_type": "markdown",
   "id": "ead4a1d1",
   "metadata": {
    "papermill": {
     "duration": 0.018539,
     "end_time": "2022-05-23T22:20:16.441526",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.422987",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "> > K-nearest neighbours"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 34,
   "id": "22c1058a",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:16.480507Z",
     "iopub.status.busy": "2022-05-23T22:20:16.478593Z",
     "iopub.status.idle": "2022-05-23T22:20:16.504646Z",
     "shell.execute_reply": "2022-05-23T22:20:16.502458Z"
    },
    "papermill": {
     "duration": 0.048302,
     "end_time": "2022-05-23T22:20:16.507741",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.459439",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "#next we will using the k-nearest neighbours in our dataset\n",
    "library(class)\n",
    "#so this classifer works in a very different ways, here we will be using the test and train split and also we will dividing our dataset into the\n",
    "#features and labels"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 35,
   "id": "ac8d7798",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:16.547901Z",
     "iopub.status.busy": "2022-05-23T22:20:16.545933Z",
     "iopub.status.idle": "2022-05-23T22:20:16.571829Z",
     "shell.execute_reply": "2022-05-23T22:20:16.570002Z"
    },
    "papermill": {
     "duration": 0.048575,
     "end_time": "2022-05-23T22:20:16.574814",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.526239",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "#need to be very careful when splitting this dataset\n",
    "train_x <- train_iris[,-c(6,7)]\n",
    "test_x <- train_iris[,6]\n",
    "train_y <- test_iris[,-c(6,7)]\n",
    "test_y<- test_iris[,6]"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 36,
   "id": "84a909d3",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:16.615666Z",
     "iopub.status.busy": "2022-05-23T22:20:16.614207Z",
     "iopub.status.idle": "2022-05-23T22:20:16.631710Z",
     "shell.execute_reply": "2022-05-23T22:20:16.629950Z"
    },
    "papermill": {
     "duration": 0.040741,
     "end_time": "2022-05-23T22:20:16.634589",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.593848",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "#in this we will be using it directly on the prediciton values to calcualte the prediction of the model\n",
    "prediction <- knn(train_x,train_y,test_x,k=5)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 37,
   "id": "70931185",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:16.674191Z",
     "iopub.status.busy": "2022-05-23T22:20:16.672528Z",
     "iopub.status.idle": "2022-05-23T22:20:16.692309Z",
     "shell.execute_reply": "2022-05-23T22:20:16.690449Z"
    },
    "papermill": {
     "duration": 0.042684,
     "end_time": "2022-05-23T22:20:16.695241",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.652557",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "                 test_y\n",
       "prediction        Iris-setosa Iris-versicolor Iris-virginica\n",
       "  Iris-setosa              19               0              0\n",
       "  Iris-versicolor           0              19              0\n",
       "  Iris-virginica            0               0             22"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#lets find out the accuracy of the model by finding out the confusion matrix\n",
    "table(prediction,test_y)\n",
    "#so it is working very good, and we are getting the accuracy at the level of tress, on thing is common is here is that , the model which use \n",
    "#distance as there as there accuracy metrics works well in this dataset\n",
    "#and the model which are based on the probability does not works that well"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "7c187221",
   "metadata": {
    "papermill": {
     "duration": 0.018245,
     "end_time": "2022-05-23T22:20:16.731549",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.713304",
     "status": "completed"
    },
    "tags": []
   },
   "source": [
    "> > Support Vector Machines"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 38,
   "id": "038a7418",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:16.771522Z",
     "iopub.status.busy": "2022-05-23T22:20:16.769959Z",
     "iopub.status.idle": "2022-05-23T22:20:16.784762Z",
     "shell.execute_reply": "2022-05-23T22:20:16.782895Z"
    },
    "papermill": {
     "duration": 0.03758,
     "end_time": "2022-05-23T22:20:16.787742",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.750162",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "#loading the library\n",
    "library(e1071)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 39,
   "id": "6b4b0f49",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:16.827408Z",
     "iopub.status.busy": "2022-05-23T22:20:16.825792Z",
     "iopub.status.idle": "2022-05-23T22:20:16.853790Z",
     "shell.execute_reply": "2022-05-23T22:20:16.851967Z"
    },
    "papermill": {
     "duration": 0.050235,
     "end_time": "2022-05-23T22:20:16.856575",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.806340",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/plain": [
       "\n",
       "Call:\n",
       "svm(formula = Species ~ ., data = tree_data, method = \"C-classification\", \n",
       "    kernel = \"radial\", gamma = 0.1, cost = 10)\n",
       "\n",
       "\n",
       "Parameters:\n",
       "   SVM-Type:  C-classification \n",
       " SVM-Kernel:  radial \n",
       "       cost:  10 \n",
       "\n",
       "Number of Support Vectors:  25\n",
       "\n",
       " ( 10 11 4 )\n",
       "\n",
       "\n",
       "Number of Classes:  3 \n",
       "\n",
       "Levels: \n",
       " Iris-setosa Iris-versicolor Iris-virginica\n",
       "\n",
       "\n"
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#the best hyper plane is considered as the largest distance between the support vector machines\n",
    "#loading the package in the library\n",
    "#two additional hyper parameters works well, gamma and the cost function\n",
    "#we are going to feed the same data we have feeded to our tree model- tree_data\n",
    "svm1 <- svm(Species~.,data = tree_data,method =\"C-classification\",kernel=\"radial\",gamma=0.1,cost=10)#we have defined the three parameter here\n",
    "#lets find out the summary of our model\n",
    "summary(svm1)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 40,
   "id": "d3149879",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:16.896706Z",
     "iopub.status.busy": "2022-05-23T22:20:16.895218Z",
     "iopub.status.idle": "2022-05-23T22:20:16.935258Z",
     "shell.execute_reply": "2022-05-23T22:20:16.933021Z"
    },
    "papermill": {
     "duration": 0.062846,
     "end_time": "2022-05-23T22:20:16.938203",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.875357",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table class=\"dataframe\">\n",
       "<caption>A matrix: 25 × 4 of type dbl</caption>\n",
       "<thead>\n",
       "\t<tr><th></th><th scope=col>SepalLengthCm</th><th scope=col>SepalWidthCm</th><th scope=col>PetalLengthCm</th><th scope=col>PetalWidthCm</th></tr>\n",
       "</thead>\n",
       "<tbody>\n",
       "\t<tr><th scope=row>134</th><td> 0.53062196</td><td>-0.61142188</td><td> 0.7794133</td><td> 0.44515838</td></tr>\n",
       "\t<tr><th scope=row>123</th><td> 2.23619253</td><td>-0.61142188</td><td> 1.6918972</td><td> 1.11964077</td></tr>\n",
       "\t<tr><th scope=row>135</th><td> 0.28696902</td><td>-1.06619849</td><td> 1.0645645</td><td> 0.31026190</td></tr>\n",
       "\t<tr><th scope=row>124</th><td> 0.53062196</td><td>-0.83881018</td><td> 0.6653528</td><td> 0.84984781</td></tr>\n",
       "\t<tr><th scope=row>128</th><td> 0.28696902</td><td>-0.15664528</td><td> 0.6653528</td><td> 0.84984781</td></tr>\n",
       "\t<tr><th scope=row>107</th><td>-1.17494862</td><td>-1.29358679</td><td> 0.4372319</td><td> 0.71495133</td></tr>\n",
       "\t<tr><th scope=row>150</th><td> 0.04331608</td><td>-0.15664528</td><td> 0.7794133</td><td> 0.84984781</td></tr>\n",
       "\t<tr><th scope=row>118</th><td> 2.23619253</td><td> 1.66246115</td><td> 1.6918972</td><td> 1.38943373</td></tr>\n",
       "\t<tr><th scope=row>130</th><td> 1.62706018</td><td>-0.15664528</td><td> 1.1786250</td><td> 0.58005486</td></tr>\n",
       "\t<tr><th scope=row>127</th><td> 0.40879549</td><td>-0.61142188</td><td> 0.6083226</td><td> 0.84984781</td></tr>\n",
       "\t<tr><th scope=row>55</th><td> 0.77427489</td><td>-0.61142188</td><td> 0.4942621</td><td> 0.44515838</td></tr>\n",
       "\t<tr><th scope=row>67</th><td>-0.32216333</td><td>-0.15664528</td><td> 0.4372319</td><td> 0.44515838</td></tr>\n",
       "\t<tr><th scope=row>78</th><td> 1.01792783</td><td>-0.15664528</td><td> 0.7223831</td><td> 0.71495133</td></tr>\n",
       "\t<tr><th scope=row>77</th><td> 1.13975430</td><td>-0.61142188</td><td> 0.6083226</td><td> 0.31026190</td></tr>\n",
       "\t<tr><th scope=row>53</th><td> 1.26158077</td><td> 0.07074303</td><td> 0.6653528</td><td> 0.44515838</td></tr>\n",
       "\t<tr><th scope=row>57</th><td> 0.53062196</td><td> 0.52551963</td><td> 0.5512923</td><td> 0.58005486</td></tr>\n",
       "\t<tr><th scope=row>58</th><td>-1.17494862</td><td>-1.52097509</td><td>-0.2471310</td><td>-0.22932401</td></tr>\n",
       "\t<tr><th scope=row>96</th><td>-0.20033686</td><td>-0.15664528</td><td> 0.2661411</td><td> 0.04046894</td></tr>\n",
       "\t<tr><th scope=row>85</th><td>-0.56581627</td><td>-0.15664528</td><td> 0.4372319</td><td> 0.44515838</td></tr>\n",
       "\t<tr><th scope=row>69</th><td> 0.40879549</td><td>-1.97575170</td><td> 0.4372319</td><td> 0.44515838</td></tr>\n",
       "\t<tr><th scope=row>79</th><td> 0.16514255</td><td>-0.38403358</td><td> 0.4372319</td><td> 0.44515838</td></tr>\n",
       "\t<tr><th scope=row>16</th><td>-0.20033686</td><td> 3.02679097</td><td>-1.2736754</td><td>-1.03870288</td></tr>\n",
       "\t<tr><th scope=row>26</th><td>-1.05312215</td><td>-0.15664528</td><td>-1.2166452</td><td>-1.30849584</td></tr>\n",
       "\t<tr><th scope=row>9</th><td>-1.78408096</td><td>-0.38403358</td><td>-1.3307056</td><td>-1.30849584</td></tr>\n",
       "\t<tr><th scope=row>24</th><td>-0.93129568</td><td> 0.52551963</td><td>-1.1596149</td><td>-0.90380640</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A matrix: 25 × 4 of type dbl\n",
       "\\begin{tabular}{r|llll}\n",
       "  & SepalLengthCm & SepalWidthCm & PetalLengthCm & PetalWidthCm\\\\\n",
       "\\hline\n",
       "\t134 &  0.53062196 & -0.61142188 &  0.7794133 &  0.44515838\\\\\n",
       "\t123 &  2.23619253 & -0.61142188 &  1.6918972 &  1.11964077\\\\\n",
       "\t135 &  0.28696902 & -1.06619849 &  1.0645645 &  0.31026190\\\\\n",
       "\t124 &  0.53062196 & -0.83881018 &  0.6653528 &  0.84984781\\\\\n",
       "\t128 &  0.28696902 & -0.15664528 &  0.6653528 &  0.84984781\\\\\n",
       "\t107 & -1.17494862 & -1.29358679 &  0.4372319 &  0.71495133\\\\\n",
       "\t150 &  0.04331608 & -0.15664528 &  0.7794133 &  0.84984781\\\\\n",
       "\t118 &  2.23619253 &  1.66246115 &  1.6918972 &  1.38943373\\\\\n",
       "\t130 &  1.62706018 & -0.15664528 &  1.1786250 &  0.58005486\\\\\n",
       "\t127 &  0.40879549 & -0.61142188 &  0.6083226 &  0.84984781\\\\\n",
       "\t55 &  0.77427489 & -0.61142188 &  0.4942621 &  0.44515838\\\\\n",
       "\t67 & -0.32216333 & -0.15664528 &  0.4372319 &  0.44515838\\\\\n",
       "\t78 &  1.01792783 & -0.15664528 &  0.7223831 &  0.71495133\\\\\n",
       "\t77 &  1.13975430 & -0.61142188 &  0.6083226 &  0.31026190\\\\\n",
       "\t53 &  1.26158077 &  0.07074303 &  0.6653528 &  0.44515838\\\\\n",
       "\t57 &  0.53062196 &  0.52551963 &  0.5512923 &  0.58005486\\\\\n",
       "\t58 & -1.17494862 & -1.52097509 & -0.2471310 & -0.22932401\\\\\n",
       "\t96 & -0.20033686 & -0.15664528 &  0.2661411 &  0.04046894\\\\\n",
       "\t85 & -0.56581627 & -0.15664528 &  0.4372319 &  0.44515838\\\\\n",
       "\t69 &  0.40879549 & -1.97575170 &  0.4372319 &  0.44515838\\\\\n",
       "\t79 &  0.16514255 & -0.38403358 &  0.4372319 &  0.44515838\\\\\n",
       "\t16 & -0.20033686 &  3.02679097 & -1.2736754 & -1.03870288\\\\\n",
       "\t26 & -1.05312215 & -0.15664528 & -1.2166452 & -1.30849584\\\\\n",
       "\t9 & -1.78408096 & -0.38403358 & -1.3307056 & -1.30849584\\\\\n",
       "\t24 & -0.93129568 &  0.52551963 & -1.1596149 & -0.90380640\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A matrix: 25 × 4 of type dbl\n",
       "\n",
       "| <!--/--> | SepalLengthCm | SepalWidthCm | PetalLengthCm | PetalWidthCm |\n",
       "|---|---|---|---|---|\n",
       "| 134 |  0.53062196 | -0.61142188 |  0.7794133 |  0.44515838 |\n",
       "| 123 |  2.23619253 | -0.61142188 |  1.6918972 |  1.11964077 |\n",
       "| 135 |  0.28696902 | -1.06619849 |  1.0645645 |  0.31026190 |\n",
       "| 124 |  0.53062196 | -0.83881018 |  0.6653528 |  0.84984781 |\n",
       "| 128 |  0.28696902 | -0.15664528 |  0.6653528 |  0.84984781 |\n",
       "| 107 | -1.17494862 | -1.29358679 |  0.4372319 |  0.71495133 |\n",
       "| 150 |  0.04331608 | -0.15664528 |  0.7794133 |  0.84984781 |\n",
       "| 118 |  2.23619253 |  1.66246115 |  1.6918972 |  1.38943373 |\n",
       "| 130 |  1.62706018 | -0.15664528 |  1.1786250 |  0.58005486 |\n",
       "| 127 |  0.40879549 | -0.61142188 |  0.6083226 |  0.84984781 |\n",
       "| 55 |  0.77427489 | -0.61142188 |  0.4942621 |  0.44515838 |\n",
       "| 67 | -0.32216333 | -0.15664528 |  0.4372319 |  0.44515838 |\n",
       "| 78 |  1.01792783 | -0.15664528 |  0.7223831 |  0.71495133 |\n",
       "| 77 |  1.13975430 | -0.61142188 |  0.6083226 |  0.31026190 |\n",
       "| 53 |  1.26158077 |  0.07074303 |  0.6653528 |  0.44515838 |\n",
       "| 57 |  0.53062196 |  0.52551963 |  0.5512923 |  0.58005486 |\n",
       "| 58 | -1.17494862 | -1.52097509 | -0.2471310 | -0.22932401 |\n",
       "| 96 | -0.20033686 | -0.15664528 |  0.2661411 |  0.04046894 |\n",
       "| 85 | -0.56581627 | -0.15664528 |  0.4372319 |  0.44515838 |\n",
       "| 69 |  0.40879549 | -1.97575170 |  0.4372319 |  0.44515838 |\n",
       "| 79 |  0.16514255 | -0.38403358 |  0.4372319 |  0.44515838 |\n",
       "| 16 | -0.20033686 |  3.02679097 | -1.2736754 | -1.03870288 |\n",
       "| 26 | -1.05312215 | -0.15664528 | -1.2166452 | -1.30849584 |\n",
       "| 9 | -1.78408096 | -0.38403358 | -1.3307056 | -1.30849584 |\n",
       "| 24 | -0.93129568 |  0.52551963 | -1.1596149 | -0.90380640 |\n",
       "\n"
      ],
      "text/plain": [
       "    SepalLengthCm SepalWidthCm PetalLengthCm PetalWidthCm\n",
       "134  0.53062196   -0.61142188   0.7794133     0.44515838 \n",
       "123  2.23619253   -0.61142188   1.6918972     1.11964077 \n",
       "135  0.28696902   -1.06619849   1.0645645     0.31026190 \n",
       "124  0.53062196   -0.83881018   0.6653528     0.84984781 \n",
       "128  0.28696902   -0.15664528   0.6653528     0.84984781 \n",
       "107 -1.17494862   -1.29358679   0.4372319     0.71495133 \n",
       "150  0.04331608   -0.15664528   0.7794133     0.84984781 \n",
       "118  2.23619253    1.66246115   1.6918972     1.38943373 \n",
       "130  1.62706018   -0.15664528   1.1786250     0.58005486 \n",
       "127  0.40879549   -0.61142188   0.6083226     0.84984781 \n",
       "55   0.77427489   -0.61142188   0.4942621     0.44515838 \n",
       "67  -0.32216333   -0.15664528   0.4372319     0.44515838 \n",
       "78   1.01792783   -0.15664528   0.7223831     0.71495133 \n",
       "77   1.13975430   -0.61142188   0.6083226     0.31026190 \n",
       "53   1.26158077    0.07074303   0.6653528     0.44515838 \n",
       "57   0.53062196    0.52551963   0.5512923     0.58005486 \n",
       "58  -1.17494862   -1.52097509  -0.2471310    -0.22932401 \n",
       "96  -0.20033686   -0.15664528   0.2661411     0.04046894 \n",
       "85  -0.56581627   -0.15664528   0.4372319     0.44515838 \n",
       "69   0.40879549   -1.97575170   0.4372319     0.44515838 \n",
       "79   0.16514255   -0.38403358   0.4372319     0.44515838 \n",
       "16  -0.20033686    3.02679097  -1.2736754    -1.03870288 \n",
       "26  -1.05312215   -0.15664528  -1.2166452    -1.30849584 \n",
       "9   -1.78408096   -0.38403358  -1.3307056    -1.30849584 \n",
       "24  -0.93129568    0.52551963  -1.1596149    -0.90380640 "
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "#finding out the support vectors \n",
    "svm1$SV #there are total 23 supoort vectors in our model"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 41,
   "id": "0d2e299e",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:16.977675Z",
     "iopub.status.busy": "2022-05-23T22:20:16.975985Z",
     "iopub.status.idle": "2022-05-23T22:20:17.110605Z",
     "shell.execute_reply": "2022-05-23T22:20:17.107441Z"
    },
    "papermill": {
     "duration": 0.157564,
     "end_time": "2022-05-23T22:20:17.114264",
     "exception": false,
     "start_time": "2022-05-23T22:20:16.956700",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "image/png": "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\nHxPsuIgJnSauAAAAAElFTkSuQmCC"
     },
     "metadata": {
      "image/png": {
       "height": 420,
       "width": 420
      }
     },
     "output_type": "display_data"
    }
   ],
   "source": [
    "#let's try to plot it and find out it by plotting the model\n",
    "plot(svm1,tree_data,PetalWidthCm ~ PetalLengthCm,slice = list(SepalWidthCm=3,SepalLengthCm=4))"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 42,
   "id": "0e37a547",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:17.158473Z",
     "iopub.status.busy": "2022-05-23T22:20:17.156961Z",
     "iopub.status.idle": "2022-05-23T22:20:17.193751Z",
     "shell.execute_reply": "2022-05-23T22:20:17.191691Z"
    },
    "papermill": {
     "duration": 0.061902,
     "end_time": "2022-05-23T22:20:17.196664",
     "exception": false,
     "start_time": "2022-05-23T22:20:17.134762",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "library(neuralnet)\n",
    "nn1_iristrain <- train_iris[,-7]"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 43,
   "id": "ce79e7b0",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:17.240299Z",
     "iopub.status.busy": "2022-05-23T22:20:17.238699Z",
     "iopub.status.idle": "2022-05-23T22:20:17.253637Z",
     "shell.execute_reply": "2022-05-23T22:20:17.251765Z"
    },
    "papermill": {
     "duration": 0.039597,
     "end_time": "2022-05-23T22:20:17.256503",
     "exception": false,
     "start_time": "2022-05-23T22:20:17.216906",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "nn1_iristrain <- cbind(nn1_iristrain, train_iris$Species ==\"Iris-setosa\")"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 44,
   "id": "5eddc026",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:17.299677Z",
     "iopub.status.busy": "2022-05-23T22:20:17.297942Z",
     "iopub.status.idle": "2022-05-23T22:20:17.316496Z",
     "shell.execute_reply": "2022-05-23T22:20:17.314767Z"
    },
    "papermill": {
     "duration": 0.042918,
     "end_time": "2022-05-23T22:20:17.319306",
     "exception": false,
     "start_time": "2022-05-23T22:20:17.276388",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "nn1_iristrain <- cbind(nn1_iristrain, train_iris$Species ==\"Iris-versicolor\")"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 45,
   "id": "d7d5695b",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:17.363296Z",
     "iopub.status.busy": "2022-05-23T22:20:17.361671Z",
     "iopub.status.idle": "2022-05-23T22:20:17.375475Z",
     "shell.execute_reply": "2022-05-23T22:20:17.373808Z"
    },
    "papermill": {
     "duration": 0.038139,
     "end_time": "2022-05-23T22:20:17.378153",
     "exception": false,
     "start_time": "2022-05-23T22:20:17.340014",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "nn1_iristrain <- cbind(nn1_iristrain, train_iris$Species ==\"Iris-virginica\")"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 46,
   "id": "4f6e29bb",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:17.420497Z",
     "iopub.status.busy": "2022-05-23T22:20:17.418993Z",
     "iopub.status.idle": "2022-05-23T22:20:17.436786Z",
     "shell.execute_reply": "2022-05-23T22:20:17.435000Z"
    },
    "papermill": {
     "duration": 0.04158,
     "end_time": "2022-05-23T22:20:17.439625",
     "exception": false,
     "start_time": "2022-05-23T22:20:17.398045",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "names(nn1_iristrain)[7] <- \"setosa\" \n",
    "names(nn1_iristrain)[8] <- \"versicolor\" \n",
    "names(nn1_iristrain)[9] <- \"virginica\" "
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 47,
   "id": "cda75d39",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:17.483886Z",
     "iopub.status.busy": "2022-05-23T22:20:17.482357Z",
     "iopub.status.idle": "2022-05-23T22:20:17.602286Z",
     "shell.execute_reply": "2022-05-23T22:20:17.595958Z"
    },
    "papermill": {
     "duration": 0.145185,
     "end_time": "2022-05-23T22:20:17.605392",
     "exception": false,
     "start_time": "2022-05-23T22:20:17.460207",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [
    {
     "data": {
      "text/html": [
       "<table class=\"dataframe\">\n",
       "<caption>A data.frame: 90 × 9</caption>\n",
       "<thead>\n",
       "\t<tr><th></th><th scope=col>Id</th><th scope=col>SepalLengthCm</th><th scope=col>SepalWidthCm</th><th scope=col>PetalLengthCm</th><th scope=col>PetalWidthCm</th><th scope=col>Species</th><th scope=col>setosa</th><th scope=col>versicolor</th><th scope=col>virginica</th></tr>\n",
       "\t<tr><th></th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;lgl&gt;</th><th scope=col>&lt;lgl&gt;</th><th scope=col>&lt;lgl&gt;</th></tr>\n",
       "</thead>\n",
       "<tbody>\n",
       "\t<tr><th scope=row>142</th><td>142</td><td>6.9</td><td>3.1</td><td>5.1</td><td>2.3</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>55</th><td> 55</td><td>6.5</td><td>2.8</td><td>4.6</td><td>1.5</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>66</th><td> 66</td><td>6.7</td><td>3.1</td><td>4.4</td><td>1.4</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>39</th><td> 39</td><td>4.4</td><td>3.0</td><td>1.3</td><td>0.2</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>60</th><td> 60</td><td>5.2</td><td>2.7</td><td>3.9</td><td>1.4</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>2</th><td>  2</td><td>4.9</td><td>3.0</td><td>1.4</td><td>0.2</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>134</th><td>134</td><td>6.3</td><td>2.8</td><td>5.1</td><td>1.5</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>67</th><td> 67</td><td>5.6</td><td>3.0</td><td>4.5</td><td>1.5</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>44</th><td> 44</td><td>5.0</td><td>3.5</td><td>1.6</td><td>0.6</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>103</th><td>103</td><td>7.1</td><td>3.0</td><td>5.9</td><td>2.1</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>40</th><td> 40</td><td>5.1</td><td>3.4</td><td>1.5</td><td>0.2</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>63</th><td> 63</td><td>6.0</td><td>2.2</td><td>4.0</td><td>1.0</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>87</th><td> 87</td><td>6.7</td><td>3.1</td><td>4.7</td><td>1.5</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>75</th><td> 75</td><td>6.4</td><td>2.9</td><td>4.3</td><td>1.3</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>90</th><td> 90</td><td>5.5</td><td>2.5</td><td>4.0</td><td>1.3</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>70</th><td> 70</td><td>5.6</td><td>2.5</td><td>3.9</td><td>1.1</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>93</th><td> 93</td><td>5.8</td><td>2.6</td><td>4.0</td><td>1.2</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>78</th><td> 78</td><td>6.7</td><td>3.0</td><td>5.0</td><td>1.7</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>123</th><td>123</td><td>7.7</td><td>2.8</td><td>6.7</td><td>2.0</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>16</th><td> 16</td><td>5.7</td><td>4.4</td><td>1.5</td><td>0.4</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>135</th><td>135</td><td>6.1</td><td>2.6</td><td>5.6</td><td>1.4</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>35</th><td> 35</td><td>4.9</td><td>3.1</td><td>1.5</td><td>0.1</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>113</th><td>113</td><td>6.8</td><td>3.0</td><td>5.5</td><td>2.1</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>74</th><td> 74</td><td>6.1</td><td>2.8</td><td>4.7</td><td>1.2</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>124</th><td>124</td><td>6.3</td><td>2.7</td><td>4.9</td><td>1.8</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>117</th><td>117</td><td>6.5</td><td>3.0</td><td>5.5</td><td>1.8</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>26</th><td> 26</td><td>5.0</td><td>3.0</td><td>1.6</td><td>0.2</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>10</th><td> 10</td><td>4.9</td><td>3.1</td><td>1.5</td><td>0.1</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>131</th><td>131</td><td>7.4</td><td>2.8</td><td>6.1</td><td>1.9</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>68</th><td> 68</td><td>5.8</td><td>2.7</td><td>4.1</td><td>1.0</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>⋮</th><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td><td>⋮</td></tr>\n",
       "\t<tr><th scope=row>100</th><td>100</td><td>5.7</td><td>2.8</td><td>4.1</td><td>1.3</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>7</th><td>  7</td><td>4.6</td><td>3.4</td><td>1.4</td><td>0.3</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>127</th><td>127</td><td>6.2</td><td>2.8</td><td>4.8</td><td>1.8</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>89</th><td> 89</td><td>5.6</td><td>3.0</td><td>4.1</td><td>1.3</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>47</th><td> 47</td><td>5.1</td><td>3.8</td><td>1.6</td><td>0.2</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>29</th><td> 29</td><td>5.2</td><td>3.4</td><td>1.4</td><td>0.2</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>61</th><td> 61</td><td>5.0</td><td>2.0</td><td>3.5</td><td>1.0</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>3</th><td>  3</td><td>4.7</td><td>3.2</td><td>1.3</td><td>0.2</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>6</th><td>  6</td><td>5.4</td><td>3.9</td><td>1.7</td><td>0.4</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>48</th><td> 48</td><td>4.6</td><td>3.2</td><td>1.4</td><td>0.2</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>34</th><td> 34</td><td>5.5</td><td>4.2</td><td>1.4</td><td>0.2</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>85</th><td> 85</td><td>5.4</td><td>3.0</td><td>4.5</td><td>1.5</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>92</th><td> 92</td><td>6.1</td><td>3.0</td><td>4.6</td><td>1.4</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>136</th><td>136</td><td>7.7</td><td>3.0</td><td>6.1</td><td>2.3</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>54</th><td> 54</td><td>5.5</td><td>2.3</td><td>4.0</td><td>1.3</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>144</th><td>144</td><td>6.8</td><td>3.2</td><td>5.9</td><td>2.3</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>5</th><td>  5</td><td>5.0</td><td>3.6</td><td>1.4</td><td>0.2</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>17</th><td> 17</td><td>5.4</td><td>3.9</td><td>1.3</td><td>0.4</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>12</th><td> 12</td><td>4.8</td><td>3.4</td><td>1.6</td><td>0.2</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>69</th><td> 69</td><td>6.2</td><td>2.2</td><td>4.5</td><td>1.5</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>112</th><td>112</td><td>6.4</td><td>2.7</td><td>5.3</td><td>1.9</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>88</th><td> 88</td><td>6.3</td><td>2.3</td><td>4.4</td><td>1.3</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>62</th><td> 62</td><td>5.9</td><td>3.0</td><td>4.2</td><td>1.5</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>133</th><td>133</td><td>6.4</td><td>2.8</td><td>5.6</td><td>2.2</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>146</th><td>146</td><td>6.7</td><td>3.0</td><td>5.2</td><td>2.3</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "\t<tr><th scope=row>18</th><td> 18</td><td>5.1</td><td>3.5</td><td>1.4</td><td>0.3</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>79</th><td> 79</td><td>6.0</td><td>2.9</td><td>4.5</td><td>1.5</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>15</th><td> 15</td><td>5.8</td><td>4.0</td><td>1.2</td><td>0.2</td><td>Iris-setosa    </td><td> TRUE</td><td>FALSE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>97</th><td> 97</td><td>5.7</td><td>2.9</td><td>4.2</td><td>1.3</td><td>Iris-versicolor</td><td>FALSE</td><td> TRUE</td><td>FALSE</td></tr>\n",
       "\t<tr><th scope=row>137</th><td>137</td><td>6.3</td><td>3.4</td><td>5.6</td><td>2.4</td><td>Iris-virginica </td><td>FALSE</td><td>FALSE</td><td> TRUE</td></tr>\n",
       "</tbody>\n",
       "</table>\n"
      ],
      "text/latex": [
       "A data.frame: 90 × 9\n",
       "\\begin{tabular}{r|lllllllll}\n",
       "  & Id & SepalLengthCm & SepalWidthCm & PetalLengthCm & PetalWidthCm & Species & setosa & versicolor & virginica\\\\\n",
       "  & <int> & <dbl> & <dbl> & <dbl> & <dbl> & <chr> & <lgl> & <lgl> & <lgl>\\\\\n",
       "\\hline\n",
       "\t142 & 142 & 6.9 & 3.1 & 5.1 & 2.3 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t55 &  55 & 6.5 & 2.8 & 4.6 & 1.5 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t66 &  66 & 6.7 & 3.1 & 4.4 & 1.4 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t39 &  39 & 4.4 & 3.0 & 1.3 & 0.2 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t60 &  60 & 5.2 & 2.7 & 3.9 & 1.4 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t2 &   2 & 4.9 & 3.0 & 1.4 & 0.2 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t134 & 134 & 6.3 & 2.8 & 5.1 & 1.5 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t67 &  67 & 5.6 & 3.0 & 4.5 & 1.5 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t44 &  44 & 5.0 & 3.5 & 1.6 & 0.6 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t103 & 103 & 7.1 & 3.0 & 5.9 & 2.1 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t40 &  40 & 5.1 & 3.4 & 1.5 & 0.2 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t63 &  63 & 6.0 & 2.2 & 4.0 & 1.0 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t87 &  87 & 6.7 & 3.1 & 4.7 & 1.5 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t75 &  75 & 6.4 & 2.9 & 4.3 & 1.3 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t90 &  90 & 5.5 & 2.5 & 4.0 & 1.3 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t70 &  70 & 5.6 & 2.5 & 3.9 & 1.1 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t93 &  93 & 5.8 & 2.6 & 4.0 & 1.2 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t78 &  78 & 6.7 & 3.0 & 5.0 & 1.7 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t123 & 123 & 7.7 & 2.8 & 6.7 & 2.0 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t16 &  16 & 5.7 & 4.4 & 1.5 & 0.4 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t135 & 135 & 6.1 & 2.6 & 5.6 & 1.4 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t35 &  35 & 4.9 & 3.1 & 1.5 & 0.1 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t113 & 113 & 6.8 & 3.0 & 5.5 & 2.1 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t74 &  74 & 6.1 & 2.8 & 4.7 & 1.2 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t124 & 124 & 6.3 & 2.7 & 4.9 & 1.8 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t117 & 117 & 6.5 & 3.0 & 5.5 & 1.8 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t26 &  26 & 5.0 & 3.0 & 1.6 & 0.2 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t10 &  10 & 4.9 & 3.1 & 1.5 & 0.1 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t131 & 131 & 7.4 & 2.8 & 6.1 & 1.9 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t68 &  68 & 5.8 & 2.7 & 4.1 & 1.0 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t⋮ & ⋮ & ⋮ & ⋮ & ⋮ & ⋮ & ⋮ & ⋮ & ⋮ & ⋮\\\\\n",
       "\t100 & 100 & 5.7 & 2.8 & 4.1 & 1.3 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t7 &   7 & 4.6 & 3.4 & 1.4 & 0.3 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t127 & 127 & 6.2 & 2.8 & 4.8 & 1.8 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t89 &  89 & 5.6 & 3.0 & 4.1 & 1.3 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t47 &  47 & 5.1 & 3.8 & 1.6 & 0.2 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t29 &  29 & 5.2 & 3.4 & 1.4 & 0.2 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t61 &  61 & 5.0 & 2.0 & 3.5 & 1.0 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t3 &   3 & 4.7 & 3.2 & 1.3 & 0.2 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t6 &   6 & 5.4 & 3.9 & 1.7 & 0.4 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t48 &  48 & 4.6 & 3.2 & 1.4 & 0.2 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t34 &  34 & 5.5 & 4.2 & 1.4 & 0.2 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t85 &  85 & 5.4 & 3.0 & 4.5 & 1.5 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t92 &  92 & 6.1 & 3.0 & 4.6 & 1.4 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t136 & 136 & 7.7 & 3.0 & 6.1 & 2.3 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t54 &  54 & 5.5 & 2.3 & 4.0 & 1.3 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t144 & 144 & 6.8 & 3.2 & 5.9 & 2.3 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t5 &   5 & 5.0 & 3.6 & 1.4 & 0.2 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t17 &  17 & 5.4 & 3.9 & 1.3 & 0.4 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t12 &  12 & 4.8 & 3.4 & 1.6 & 0.2 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t69 &  69 & 6.2 & 2.2 & 4.5 & 1.5 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t112 & 112 & 6.4 & 2.7 & 5.3 & 1.9 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t88 &  88 & 6.3 & 2.3 & 4.4 & 1.3 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t62 &  62 & 5.9 & 3.0 & 4.2 & 1.5 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t133 & 133 & 6.4 & 2.8 & 5.6 & 2.2 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t146 & 146 & 6.7 & 3.0 & 5.2 & 2.3 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\t18 &  18 & 5.1 & 3.5 & 1.4 & 0.3 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t79 &  79 & 6.0 & 2.9 & 4.5 & 1.5 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t15 &  15 & 5.8 & 4.0 & 1.2 & 0.2 & Iris-setosa     &  TRUE & FALSE & FALSE\\\\\n",
       "\t97 &  97 & 5.7 & 2.9 & 4.2 & 1.3 & Iris-versicolor & FALSE &  TRUE & FALSE\\\\\n",
       "\t137 & 137 & 6.3 & 3.4 & 5.6 & 2.4 & Iris-virginica  & FALSE & FALSE &  TRUE\\\\\n",
       "\\end{tabular}\n"
      ],
      "text/markdown": [
       "\n",
       "A data.frame: 90 × 9\n",
       "\n",
       "| <!--/--> | Id &lt;int&gt; | SepalLengthCm &lt;dbl&gt; | SepalWidthCm &lt;dbl&gt; | PetalLengthCm &lt;dbl&gt; | PetalWidthCm &lt;dbl&gt; | Species &lt;chr&gt; | setosa &lt;lgl&gt; | versicolor &lt;lgl&gt; | virginica &lt;lgl&gt; |\n",
       "|---|---|---|---|---|---|---|---|---|---|\n",
       "| 142 | 142 | 6.9 | 3.1 | 5.1 | 2.3 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 55 |  55 | 6.5 | 2.8 | 4.6 | 1.5 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 66 |  66 | 6.7 | 3.1 | 4.4 | 1.4 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 39 |  39 | 4.4 | 3.0 | 1.3 | 0.2 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 60 |  60 | 5.2 | 2.7 | 3.9 | 1.4 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 2 |   2 | 4.9 | 3.0 | 1.4 | 0.2 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 134 | 134 | 6.3 | 2.8 | 5.1 | 1.5 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 67 |  67 | 5.6 | 3.0 | 4.5 | 1.5 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 44 |  44 | 5.0 | 3.5 | 1.6 | 0.6 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 103 | 103 | 7.1 | 3.0 | 5.9 | 2.1 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 40 |  40 | 5.1 | 3.4 | 1.5 | 0.2 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 63 |  63 | 6.0 | 2.2 | 4.0 | 1.0 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 87 |  87 | 6.7 | 3.1 | 4.7 | 1.5 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 75 |  75 | 6.4 | 2.9 | 4.3 | 1.3 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 90 |  90 | 5.5 | 2.5 | 4.0 | 1.3 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 70 |  70 | 5.6 | 2.5 | 3.9 | 1.1 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 93 |  93 | 5.8 | 2.6 | 4.0 | 1.2 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 78 |  78 | 6.7 | 3.0 | 5.0 | 1.7 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 123 | 123 | 7.7 | 2.8 | 6.7 | 2.0 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 16 |  16 | 5.7 | 4.4 | 1.5 | 0.4 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 135 | 135 | 6.1 | 2.6 | 5.6 | 1.4 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 35 |  35 | 4.9 | 3.1 | 1.5 | 0.1 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 113 | 113 | 6.8 | 3.0 | 5.5 | 2.1 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 74 |  74 | 6.1 | 2.8 | 4.7 | 1.2 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 124 | 124 | 6.3 | 2.7 | 4.9 | 1.8 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 117 | 117 | 6.5 | 3.0 | 5.5 | 1.8 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 26 |  26 | 5.0 | 3.0 | 1.6 | 0.2 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 10 |  10 | 4.9 | 3.1 | 1.5 | 0.1 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 131 | 131 | 7.4 | 2.8 | 6.1 | 1.9 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 68 |  68 | 5.8 | 2.7 | 4.1 | 1.0 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| ⋮ | ⋮ | ⋮ | ⋮ | ⋮ | ⋮ | ⋮ | ⋮ | ⋮ | ⋮ |\n",
       "| 100 | 100 | 5.7 | 2.8 | 4.1 | 1.3 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 7 |   7 | 4.6 | 3.4 | 1.4 | 0.3 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 127 | 127 | 6.2 | 2.8 | 4.8 | 1.8 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 89 |  89 | 5.6 | 3.0 | 4.1 | 1.3 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 47 |  47 | 5.1 | 3.8 | 1.6 | 0.2 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 29 |  29 | 5.2 | 3.4 | 1.4 | 0.2 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 61 |  61 | 5.0 | 2.0 | 3.5 | 1.0 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 3 |   3 | 4.7 | 3.2 | 1.3 | 0.2 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 6 |   6 | 5.4 | 3.9 | 1.7 | 0.4 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 48 |  48 | 4.6 | 3.2 | 1.4 | 0.2 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 34 |  34 | 5.5 | 4.2 | 1.4 | 0.2 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 85 |  85 | 5.4 | 3.0 | 4.5 | 1.5 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 92 |  92 | 6.1 | 3.0 | 4.6 | 1.4 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 136 | 136 | 7.7 | 3.0 | 6.1 | 2.3 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 54 |  54 | 5.5 | 2.3 | 4.0 | 1.3 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 144 | 144 | 6.8 | 3.2 | 5.9 | 2.3 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 5 |   5 | 5.0 | 3.6 | 1.4 | 0.2 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 17 |  17 | 5.4 | 3.9 | 1.3 | 0.4 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 12 |  12 | 4.8 | 3.4 | 1.6 | 0.2 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 69 |  69 | 6.2 | 2.2 | 4.5 | 1.5 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 112 | 112 | 6.4 | 2.7 | 5.3 | 1.9 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 88 |  88 | 6.3 | 2.3 | 4.4 | 1.3 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 62 |  62 | 5.9 | 3.0 | 4.2 | 1.5 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 133 | 133 | 6.4 | 2.8 | 5.6 | 2.2 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 146 | 146 | 6.7 | 3.0 | 5.2 | 2.3 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "| 18 |  18 | 5.1 | 3.5 | 1.4 | 0.3 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 79 |  79 | 6.0 | 2.9 | 4.5 | 1.5 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 15 |  15 | 5.8 | 4.0 | 1.2 | 0.2 | Iris-setosa     |  TRUE | FALSE | FALSE |\n",
       "| 97 |  97 | 5.7 | 2.9 | 4.2 | 1.3 | Iris-versicolor | FALSE |  TRUE | FALSE |\n",
       "| 137 | 137 | 6.3 | 3.4 | 5.6 | 2.4 | Iris-virginica  | FALSE | FALSE |  TRUE |\n",
       "\n"
      ],
      "text/plain": [
       "    Id  SepalLengthCm SepalWidthCm PetalLengthCm PetalWidthCm Species        \n",
       "142 142 6.9           3.1          5.1           2.3          Iris-virginica \n",
       "55   55 6.5           2.8          4.6           1.5          Iris-versicolor\n",
       "66   66 6.7           3.1          4.4           1.4          Iris-versicolor\n",
       "39   39 4.4           3.0          1.3           0.2          Iris-setosa    \n",
       "60   60 5.2           2.7          3.9           1.4          Iris-versicolor\n",
       "2     2 4.9           3.0          1.4           0.2          Iris-setosa    \n",
       "134 134 6.3           2.8          5.1           1.5          Iris-virginica \n",
       "67   67 5.6           3.0          4.5           1.5          Iris-versicolor\n",
       "44   44 5.0           3.5          1.6           0.6          Iris-setosa    \n",
       "103 103 7.1           3.0          5.9           2.1          Iris-virginica \n",
       "40   40 5.1           3.4          1.5           0.2          Iris-setosa    \n",
       "63   63 6.0           2.2          4.0           1.0          Iris-versicolor\n",
       "87   87 6.7           3.1          4.7           1.5          Iris-versicolor\n",
       "75   75 6.4           2.9          4.3           1.3          Iris-versicolor\n",
       "90   90 5.5           2.5          4.0           1.3          Iris-versicolor\n",
       "70   70 5.6           2.5          3.9           1.1          Iris-versicolor\n",
       "93   93 5.8           2.6          4.0           1.2          Iris-versicolor\n",
       "78   78 6.7           3.0          5.0           1.7          Iris-versicolor\n",
       "123 123 7.7           2.8          6.7           2.0          Iris-virginica \n",
       "16   16 5.7           4.4          1.5           0.4          Iris-setosa    \n",
       "135 135 6.1           2.6          5.6           1.4          Iris-virginica \n",
       "35   35 4.9           3.1          1.5           0.1          Iris-setosa    \n",
       "113 113 6.8           3.0          5.5           2.1          Iris-virginica \n",
       "74   74 6.1           2.8          4.7           1.2          Iris-versicolor\n",
       "124 124 6.3           2.7          4.9           1.8          Iris-virginica \n",
       "117 117 6.5           3.0          5.5           1.8          Iris-virginica \n",
       "26   26 5.0           3.0          1.6           0.2          Iris-setosa    \n",
       "10   10 4.9           3.1          1.5           0.1          Iris-setosa    \n",
       "131 131 7.4           2.8          6.1           1.9          Iris-virginica \n",
       "68   68 5.8           2.7          4.1           1.0          Iris-versicolor\n",
       "⋮   ⋮   ⋮             ⋮            ⋮             ⋮            ⋮              \n",
       "100 100 5.7           2.8          4.1           1.3          Iris-versicolor\n",
       "7     7 4.6           3.4          1.4           0.3          Iris-setosa    \n",
       "127 127 6.2           2.8          4.8           1.8          Iris-virginica \n",
       "89   89 5.6           3.0          4.1           1.3          Iris-versicolor\n",
       "47   47 5.1           3.8          1.6           0.2          Iris-setosa    \n",
       "29   29 5.2           3.4          1.4           0.2          Iris-setosa    \n",
       "61   61 5.0           2.0          3.5           1.0          Iris-versicolor\n",
       "3     3 4.7           3.2          1.3           0.2          Iris-setosa    \n",
       "6     6 5.4           3.9          1.7           0.4          Iris-setosa    \n",
       "48   48 4.6           3.2          1.4           0.2          Iris-setosa    \n",
       "34   34 5.5           4.2          1.4           0.2          Iris-setosa    \n",
       "85   85 5.4           3.0          4.5           1.5          Iris-versicolor\n",
       "92   92 6.1           3.0          4.6           1.4          Iris-versicolor\n",
       "136 136 7.7           3.0          6.1           2.3          Iris-virginica \n",
       "54   54 5.5           2.3          4.0           1.3          Iris-versicolor\n",
       "144 144 6.8           3.2          5.9           2.3          Iris-virginica \n",
       "5     5 5.0           3.6          1.4           0.2          Iris-setosa    \n",
       "17   17 5.4           3.9          1.3           0.4          Iris-setosa    \n",
       "12   12 4.8           3.4          1.6           0.2          Iris-setosa    \n",
       "69   69 6.2           2.2          4.5           1.5          Iris-versicolor\n",
       "112 112 6.4           2.7          5.3           1.9          Iris-virginica \n",
       "88   88 6.3           2.3          4.4           1.3          Iris-versicolor\n",
       "62   62 5.9           3.0          4.2           1.5          Iris-versicolor\n",
       "133 133 6.4           2.8          5.6           2.2          Iris-virginica \n",
       "146 146 6.7           3.0          5.2           2.3          Iris-virginica \n",
       "18   18 5.1           3.5          1.4           0.3          Iris-setosa    \n",
       "79   79 6.0           2.9          4.5           1.5          Iris-versicolor\n",
       "15   15 5.8           4.0          1.2           0.2          Iris-setosa    \n",
       "97   97 5.7           2.9          4.2           1.3          Iris-versicolor\n",
       "137 137 6.3           3.4          5.6           2.4          Iris-virginica \n",
       "    setosa versicolor virginica\n",
       "142 FALSE  FALSE       TRUE    \n",
       "55  FALSE   TRUE      FALSE    \n",
       "66  FALSE   TRUE      FALSE    \n",
       "39   TRUE  FALSE      FALSE    \n",
       "60  FALSE   TRUE      FALSE    \n",
       "2    TRUE  FALSE      FALSE    \n",
       "134 FALSE  FALSE       TRUE    \n",
       "67  FALSE   TRUE      FALSE    \n",
       "44   TRUE  FALSE      FALSE    \n",
       "103 FALSE  FALSE       TRUE    \n",
       "40   TRUE  FALSE      FALSE    \n",
       "63  FALSE   TRUE      FALSE    \n",
       "87  FALSE   TRUE      FALSE    \n",
       "75  FALSE   TRUE      FALSE    \n",
       "90  FALSE   TRUE      FALSE    \n",
       "70  FALSE   TRUE      FALSE    \n",
       "93  FALSE   TRUE      FALSE    \n",
       "78  FALSE   TRUE      FALSE    \n",
       "123 FALSE  FALSE       TRUE    \n",
       "16   TRUE  FALSE      FALSE    \n",
       "135 FALSE  FALSE       TRUE    \n",
       "35   TRUE  FALSE      FALSE    \n",
       "113 FALSE  FALSE       TRUE    \n",
       "74  FALSE   TRUE      FALSE    \n",
       "124 FALSE  FALSE       TRUE    \n",
       "117 FALSE  FALSE       TRUE    \n",
       "26   TRUE  FALSE      FALSE    \n",
       "10   TRUE  FALSE      FALSE    \n",
       "131 FALSE  FALSE       TRUE    \n",
       "68  FALSE   TRUE      FALSE    \n",
       "⋮   ⋮      ⋮          ⋮        \n",
       "100 FALSE   TRUE      FALSE    \n",
       "7    TRUE  FALSE      FALSE    \n",
       "127 FALSE  FALSE       TRUE    \n",
       "89  FALSE   TRUE      FALSE    \n",
       "47   TRUE  FALSE      FALSE    \n",
       "29   TRUE  FALSE      FALSE    \n",
       "61  FALSE   TRUE      FALSE    \n",
       "3    TRUE  FALSE      FALSE    \n",
       "6    TRUE  FALSE      FALSE    \n",
       "48   TRUE  FALSE      FALSE    \n",
       "34   TRUE  FALSE      FALSE    \n",
       "85  FALSE   TRUE      FALSE    \n",
       "92  FALSE   TRUE      FALSE    \n",
       "136 FALSE  FALSE       TRUE    \n",
       "54  FALSE   TRUE      FALSE    \n",
       "144 FALSE  FALSE       TRUE    \n",
       "5    TRUE  FALSE      FALSE    \n",
       "17   TRUE  FALSE      FALSE    \n",
       "12   TRUE  FALSE      FALSE    \n",
       "69  FALSE   TRUE      FALSE    \n",
       "112 FALSE  FALSE       TRUE    \n",
       "88  FALSE   TRUE      FALSE    \n",
       "62  FALSE   TRUE      FALSE    \n",
       "133 FALSE  FALSE       TRUE    \n",
       "146 FALSE  FALSE       TRUE    \n",
       "18   TRUE  FALSE      FALSE    \n",
       "79  FALSE   TRUE      FALSE    \n",
       "15   TRUE  FALSE      FALSE    \n",
       "97  FALSE   TRUE      FALSE    \n",
       "137 FALSE  FALSE       TRUE    "
      ]
     },
     "metadata": {},
     "output_type": "display_data"
    }
   ],
   "source": [
    "nn1_iristrain #hence we have seperated the class in this segment"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 48,
   "id": "5eff8c76",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:17.651922Z",
     "iopub.status.busy": "2022-05-23T22:20:17.650418Z",
     "iopub.status.idle": "2022-05-23T22:20:21.193751Z",
     "shell.execute_reply": "2022-05-23T22:20:21.191538Z"
    },
    "papermill": {
     "duration": 3.569549,
     "end_time": "2022-05-23T22:20:21.197250",
     "exception": false,
     "start_time": "2022-05-23T22:20:17.627701",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "nn1 <- neuralnet(setosa+versicolor+virginica ~ SepalLengthCm +SepalWidthCm + PetalLengthCm + PetalWidthCm,\n",
    "                data=nn1_iristrain,hidden=c(4))"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 49,
   "id": "fa308dc5",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:21.244191Z",
     "iopub.status.busy": "2022-05-23T22:20:21.242374Z",
     "iopub.status.idle": "2022-05-23T22:20:21.373839Z",
     "shell.execute_reply": "2022-05-23T22:20:21.371464Z"
    },
    "papermill": {
     "duration": 0.157918,
     "end_time": "2022-05-23T22:20:21.376792",
     "exception": false,
     "start_time": "2022-05-23T22:20:21.218874",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "plot(nn1)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 50,
   "id": "f2f2083a",
   "metadata": {
    "execution": {
     "iopub.execute_input": "2022-05-23T22:20:21.422272Z",
     "iopub.status.busy": "2022-05-23T22:20:21.420495Z",
     "iopub.status.idle": "2022-05-23T22:20:21.438319Z",
     "shell.execute_reply": "2022-05-23T22:20:21.436384Z"
    },
    "papermill": {
     "duration": 0.042551,
     "end_time": "2022-05-23T22:20:21.441236",
     "exception": false,
     "start_time": "2022-05-23T22:20:21.398685",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": [
    "prediction <- compute(nn1,test_iris[,-6])\n",
    "prediction <- prediction$net.result"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "id": "469872b0",
   "metadata": {
    "papermill": {
     "duration": 0.022142,
     "end_time": "2022-05-23T22:20:21.485115",
     "exception": false,
     "start_time": "2022-05-23T22:20:21.462973",
     "status": "completed"
    },
    "tags": []
   },
   "outputs": [],
   "source": []
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "R",
   "language": "R",
   "name": "ir"
  },
  "language_info": {
   "codemirror_mode": "r",
   "file_extension": ".r",
   "mimetype": "text/x-r-source",
   "name": "R",
   "pygments_lexer": "r",
   "version": "4.0.5"
  },
  "papermill": {
   "default_parameters": {},
   "duration": 11.919781,
   "end_time": "2022-05-23T22:20:21.626580",
   "environment_variables": {},
   "exception": null,
   "input_path": "__notebook__.ipynb",
   "output_path": "__notebook__.ipynb",
   "parameters": {},
   "start_time": "2022-05-23T22:20:09.706799",
   "version": "2.3.4"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}
