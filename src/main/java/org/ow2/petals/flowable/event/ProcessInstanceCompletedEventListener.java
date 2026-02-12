/**
 * Copyright (c) 2015-2026 Linagora
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
package org.ow2.petals.flowable.event;

import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_FLOW_INSTANCE_ID;
import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_FLOW_STEP_ID;

import java.util.Map;

import org.flowable.common.engine.api.delegate.event.FlowableEngineEventType;
import org.flowable.common.engine.api.delegate.event.FlowableEvent;
import org.flowable.common.engine.api.delegate.event.FlowableEventListener;
import org.flowable.engine.HistoryService;
import org.flowable.engine.delegate.event.impl.FlowableEntityEventImpl;
import org.flowable.engine.history.HistoricProcessInstance;
import org.flowable.engine.history.HistoricProcessInstanceQuery;
import org.ow2.petals.component.framework.AbstractComponent;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;
import org.ow2.petals.flowable.monitoring.ProcessInstanceFlowStepEndLogData;

/**
 * The event listener fired when a process instance is completed to log a MONIT trace.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class ProcessInstanceCompletedEventListener extends AbstractProcessEventListener
        implements FlowableEventListener {

    public ProcessInstanceCompletedEventListener(final HistoryService historyService,
            final AbstractComponent component) {
        super(FlowableEngineEventType.PROCESS_COMPLETED, historyService, component);
    }

    @Override
    protected AbstractFlowLogData createLogData(final FlowableEvent event) {

        if (event instanceof FlowableEntityEventImpl) {
            final FlowableEntityEventImpl eventImpl = (FlowableEntityEventImpl) event;

            final String processInstanceId = eventImpl.getProcessInstanceId();
            this.log.fine("The process instance '" + processInstanceId + "' is completed.");

            final HistoricProcessInstanceQuery processQuery = this.historyService.createHistoricProcessInstanceQuery()
                    .processInstanceId(processInstanceId).includeProcessVariables();
            final HistoricProcessInstance processResult = processQuery.singleResult();

            final Map<String, Object> processVariables = processResult.getProcessVariables();

            final String flowInstanceId = (String) processVariables.get(VAR_PETALS_FLOW_INSTANCE_ID);
            final String flowStepId = (String) processVariables.get(VAR_PETALS_FLOW_STEP_ID);

            if (this.isFlowTracingEnabled(processVariables)) {
                return new ProcessInstanceFlowStepEndLogData(flowInstanceId, flowStepId);
            } else {
                return null;
            }
        } else {
            this.log.warning("Unexpected event implementation: " + event.getClass().getName());
            return null;
        }
    }
}
