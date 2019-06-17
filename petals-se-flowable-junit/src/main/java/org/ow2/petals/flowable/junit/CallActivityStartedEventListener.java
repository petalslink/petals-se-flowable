/**
 * Copyright (c) 2017-2019 Linagora
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
package org.ow2.petals.flowable.junit;

import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_PLACEHOLDERS;

import java.util.Map;
import java.util.logging.Logger;

import org.flowable.engine.RuntimeService;
import org.flowable.engine.common.api.delegate.event.FlowableEvent;
import org.flowable.engine.common.api.delegate.event.FlowableEventListener;
import org.flowable.engine.delegate.event.impl.FlowableProcessStartedEventImpl;
import org.flowable.engine.impl.persistence.entity.ExecutionEntity;

/**
 * Propagate placeholders into call activity steps.
 * 
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class CallActivityStartedEventListener implements FlowableEventListener {

    private final RuntimeService runtimeService;

    private final Map<String, String> placeholders;

    private final Logger log;

    public CallActivityStartedEventListener(final RuntimeService runtimeService, final Map<String, String> placeholders,
            final Logger log) {
        this.runtimeService = runtimeService;
        this.placeholders = placeholders;
        this.log = log;
    }

    @Override
    public void onEvent(final FlowableEvent event) {

        if (event instanceof FlowableProcessStartedEventImpl) {
            final FlowableProcessStartedEventImpl eventImpl = (FlowableProcessStartedEventImpl) event;

            if (eventImpl.getNestedProcessDefinitionId() != null) {

                final String callActivityInstanceId = eventImpl.getProcessInstanceId();
                final String parentInstanceId = eventImpl.getNestedProcessInstanceId();
                final ExecutionEntity callActivityEntity = (ExecutionEntity) eventImpl.getEntity();

                this.log.fine(String.format("The call activity '%s' (instance id '%s') is started from instance '%s'.",
                        callActivityEntity.getProcessDefinitionKey(), callActivityInstanceId, parentInstanceId));

                // Placeholders must be copied in the call activity scope
                this.runtimeService.setVariable(eventImpl.getExecutionId(), VAR_PETALS_PLACEHOLDERS, this.placeholders);
            }

        } else {
            this.log.warning("Unexpected event implementation: " + event.getClass().getName());
        }
    }

    @Override
    public boolean isFailOnException() {
        return false;
    }
}
