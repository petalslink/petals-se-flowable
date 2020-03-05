/**
 * Copyright (c) 2017-2020 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.rest.config;

import org.flowable.common.engine.api.FlowableTaskAlreadyClaimedException;
import org.flowable.common.rest.exception.BaseExceptionHandlerAdvice;
import org.flowable.common.rest.exception.ErrorInfo;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

/**
 * Class which defines the handler for REST exceptions. Inspired by flowable-rest-app code
 * (org.flowable.rest.conf.RestExceptionHandlerAdvice).
 * 
 * TODO remove when it is moved from flowable-rest-app to flowable-rest (if it is ever).
 * 
 * @author Victor NOEL - Linagora
 * @author Jordy CABANNES - Linagora
 */
@ControllerAdvice
public class RestExceptionHandlerAdvice extends BaseExceptionHandlerAdvice {

    @ResponseStatus(HttpStatus.CONFLICT) // 409
    @ExceptionHandler(FlowableTaskAlreadyClaimedException.class)
    @ResponseBody
    @Override
    public ErrorInfo handleTaskAlreadyClaimed(final FlowableTaskAlreadyClaimedException e) {
        return new ErrorInfo("Task was already claimed", e);
    }

}
