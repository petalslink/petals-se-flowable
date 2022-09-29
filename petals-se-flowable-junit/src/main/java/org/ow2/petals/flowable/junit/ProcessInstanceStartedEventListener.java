/**
 * Copyright (c) 2017-2022 Linagora
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

import java.util.logging.Logger;

import org.flowable.common.engine.api.delegate.event.FlowableEvent;
import org.flowable.common.engine.api.delegate.event.FlowableEventListener;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.delegate.event.impl.FlowableProcessStartedEventImpl;

/**
 * The event listener fired when a process instance is started to set placeholders.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class ProcessInstanceStartedEventListener implements FlowableEventListener {

    private final RuntimeService runtimeService;

    private final Logger log;

    public ProcessInstanceStartedEventListener(final RuntimeService runtimeService, final Logger log) {
        this.runtimeService = runtimeService;
        this.log = log;
    }

    @Override
    public void onEvent(final FlowableEvent event) {

        if (event instanceof FlowableProcessStartedEventImpl) {
            final FlowableProcessStartedEventImpl eventImpl = (FlowableProcessStartedEventImpl) event;

            if (eventImpl.getNestedProcessDefinitionId() == null) {

                final String processInstanceId = eventImpl.getProcessInstanceId();
                this.log.fine("The process instance '" + processInstanceId + "' is started.");

            } else {
                // A call activity or a sub-process is started
            }

        } else {
            this.log.warning("Unexpected event implementation: " + event.getClass().getName());
        }
    }

    @Override
    public boolean isFailOnException() {
        return false;
    }

    @Override
    public boolean isFireOnTransactionLifecycleEvent() {
        return false;
    }

    @Override
    public String getOnTransaction() {
        return null;
    }
}
