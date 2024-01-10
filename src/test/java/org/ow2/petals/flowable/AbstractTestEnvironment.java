/**
 * Copyright (c) 2014-2024 Linagora
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
package org.ow2.petals.flowable;

import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_JDBC_URL_DATABASE_FILENAME;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.logging.LogRecord;

import org.apache.mina.util.AvailablePortFinder;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.runtime.ProcessInstanceQuery;
import org.junit.Before;
import org.junit.Rule;
import org.junit.rules.TemporaryFolder;
import org.ow2.petals.commons.log.FlowLogData;
import org.ow2.petals.commons.log.TraceCode;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTest;
import org.ow2.petals.flowable.identity.file.FileIdmEngineConfigurator;
import org.ow2.petals.flowable.junit.FlowableClient;
import org.ow2.petals.flowable.utils.test.Await;
import org.ow2.petals.junit.rules.log.handler.InMemoryLogHandler;

/**
 * Abstract class for unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class AbstractTestEnvironment extends AbstractTest {

    protected static final String NATIVE_USER_SVC_CFG = "native-user";

    protected static final String NATIVE_GROUP_SVC_CFG = "native-group";

    protected static final String NATIVE_TASKS_SVC_CFG = "native-tasks";

    protected static final String NATIVE_PROCESSINSTANCES_SVC_CFG = "native-process-instances";

    protected static final String NATIVE_EXECUTIONS_SVC_CFG = "native-executions";

    protected static final InMemoryLogHandler IN_MEMORY_LOG_HANDLER = new InMemoryLogHandler();

    protected static final TemporaryFolder TEMP_FOLDER = new TemporaryFolder();

    protected static final int EMBEDDED_REST_API_HTTP_PORT = AvailablePortFinder
            .getNextAvailable(FlowableSEConstants.DEFAULT_ENGINE_REST_API_PORT);

    @Rule
    public FlowableClient flowableClient = new FlowableClient(
            new File(new File(this.getComponentUnderTest().getBaseDirectory(), "work"),
                    DEFAULT_JDBC_URL_DATABASE_FILENAME),
            new FileIdmEngineConfigurator(), VACATION_SU_HOME + "idm-engine-configurator.properties");

    protected abstract ComponentUnderTest getComponentUnderTest();

    public AbstractTestEnvironment() {
        this(null);
    }

    public AbstractTestEnvironment(final String fileIdmEngineConfiguratorCfgFile) {
        this.flowableClient = new FlowableClient(
                new File(new File(this.getComponentUnderTest().getBaseDirectory(), "work"),
                        DEFAULT_JDBC_URL_DATABASE_FILENAME),
                new FileIdmEngineConfigurator(), fileIdmEngineConfiguratorCfgFile);
    }

    /**
     * All log traces must be cleared before starting a unit test
     */
    @Before
    public void clearLogTraces() {
        IN_MEMORY_LOG_HANDLER.clear();
    }

    /**
     * @param processDefinitionKey
     *            The process definition identifier
     * @return The number of process instances that are not finished and associated to the given process definition
     */
    protected int getProcessInstanceNumber(final String processDefinitionKey) {
        final ProcessInstanceQuery processInstQuery = this.flowableClient.getRuntimeService()
                .createProcessInstanceQuery();
        final List<ProcessInstance> processInstances = processInstQuery.processDefinitionKey(processDefinitionKey)
                .list();
        assertNotNull(processInstances);
        return processInstances.size();
    }

    /**
     * Assertion to check that a process instance is running. The process instance is not finished.
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param processDefinitionKey
     *            The process definition key
     */
    protected void assertProcessInstancePending(final String processInstanceId, final String processDefinitionKey) {

        org.ow2.petals.flowable.utils.test.Assert.assertProcessInstancePending(processInstanceId, processDefinitionKey,
                this.flowableClient.getRuntimeService());
    }

    /**
     * Assertion to check that a process instance is finished.
     * 
     * @param processInstanceId
     *            The process instance identifier
     */
    protected void assertProcessInstanceFinished(final String processInstanceId) {

        org.ow2.petals.flowable.utils.test.Assert.assertProcessInstanceFinished(processInstanceId,
                this.flowableClient.getHistoryService());
    }

    /**
     * Assertion to check that a user task can be completed by a given user.
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param taskDefinitionKey
     *            The process definition key
     * @param user
     *            The task can be completed by the given user
     */
    protected void assertCurrentUserTask(final String processInstanceId, final String taskDefinitionKey,
            final String user) {

        org.ow2.petals.flowable.utils.test.Assert.assertCurrentUserTask(processInstanceId, taskDefinitionKey, user,
                this.flowableClient.getTaskService());
    }

    /**
     * Assertion to check that a user task was completed by the given user.
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param taskDefinitionKey
     *            The process definition key
     * @param user
     *            The task has been completed by the given user
     */
    protected void assertUserTaskEnded(final String processInstanceId, final String taskDefinitionKey,
            final String user) {

        org.ow2.petals.flowable.utils.test.Assert.assertUserTaskEnded(processInstanceId, taskDefinitionKey, user,
                this.flowableClient.getHistoryService());
    }

    /**
     * Wait that a process instance is put as dead letter job. If the waiting time is upper than 60s, an
     * {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the process instance to wait its placement as dead letter job.
     */
    protected void waitProcessInstanceAsDeadLetterJob(final String processInstanceId) throws InterruptedException {
        Await.waitProcessInstanceAsDeadLetterJob(processInstanceId, this.flowableClient.getManagementService(), 60);
    }

    /**
     * Wait that a process instance ends successfully. If the waiting time is upper than 60s, an {@link AssertionError}
     * is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the process instance to wait its end.
     */
    protected void waitEndOfProcessInstance(final String processInstanceId) throws InterruptedException {
        this.waitEndOfProcessInstance(processInstanceId, 60);
    }

    /**
     * Wait that a process instance ends successfully. If the waiting time is upper than the given one, an
     * {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the process instance to wait its succeeded end.
     * @param duration
     *            Waiting time in seconds before to throw a {@link AssertionError} to fail the processing
     */
    protected void waitEndOfProcessInstance(final String processInstanceId, final int duration)
            throws InterruptedException {
        Await.waitEndOfProcessInstance(processInstanceId, this.flowableClient.getHistoryService(), duration);
    }

    /**
     * Wait that a service task of a process instance ends. If the waiting time is upper than 60s, an
     * {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its end.
     * @param serviceTaskDefinitionKey
     *            The service task identifier in the process definition
     */
    protected void waitEndOfServiceTask(final String processInstanceId, final String serviceTaskDefinitionKey)
            throws InterruptedException {
        Await.waitEndOfServiceTask(processInstanceId, serviceTaskDefinitionKey, this.flowableClient.getHistoryService(),
                60);
    }

    /**
     * Wait that a user task of a process instance is assigned to a candidate user. If the waiting time is upper than
     * 60s, an {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its assignment.
     * @param taskDefinitionKey
     *            The user task identifier in the process definition
     * @param candidateUser
     *            The candidate user of the user task
     */
    protected void waitUserTaskAssignment(final String processInstanceId, final String taskDefinitionKey,
            final String candidateUser) throws InterruptedException {
        this.waitUserTaskAssignment(processInstanceId, taskDefinitionKey, candidateUser, 60);
    }

    /**
     * Wait that a user task of a process instance is assigned to a candidate user. If the waiting time is upper than
     * the given one, an {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its assignment.
     * @param taskDefinitionKey
     *            The user task identifier in the process definition
     * @param candidateUser
     *            The candidate user of the user task
     * @param duration
     *            Waiting time in seconds before to throw a {@link AssertionError} to fail the processing
     */
    protected void waitUserTaskAssignment(final String processInstanceId, final String taskDefinitionKey,
            final String candidateUser, final int duration) throws InterruptedException {

        Await.waitUserTaskAssignment(processInstanceId, taskDefinitionKey, candidateUser,
                this.flowableClient.getTaskService(), duration);
    }

    /**
     * Wait that an intermediate catch message event is ready to receive message. If the waiting time is upper than 60s,
     * an {@link AssertionError} is thrown.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its assignment.
     * @param messageEventName
     *            The message name of the intermediate catch message event
     */
    protected void waitIntermediateCatchMessageEvent(final String processInstanceId, final String messageEventName)
            throws InterruptedException {
        Await.waitIntermediateCatchMessageEvent(processInstanceId, messageEventName,
                this.flowableClient.getRuntimeService(), 60);
    }

    /**
     * Retrieve and remove the MONIT trace 'provideFlowStepBegin' associated to the given endpoint.
     * 
     * @param providerEndpoint
     *            The service provider endpoint name for which the MONIT trace is looked for.
     * @param monitLogs
     *            MONIT traces of the flow
     * @return The MONIT trace extracted matching criteria
     */
    protected LogRecord extractProviderBegin(final String providerEndpoint, final List<LogRecord> monitLogs) {
        final Iterator<LogRecord> itMonitLogs = monitLogs.iterator();
        while (itMonitLogs.hasNext()) {
            final LogRecord logRecord = itMonitLogs.next();
            final FlowLogData flowLogData = (FlowLogData) logRecord.getParameters()[0];
            if (flowLogData.get(FlowLogData.FLOW_STEP_TRACE_CODE) == TraceCode.PROVIDE_FLOW_STEP_BEGIN
                    && flowLogData.get(FlowLogData.FLOW_STEP_ENDPOINT_NAME_PROPERTY_NAME).equals(providerEndpoint)) {
                itMonitLogs.remove();
                return logRecord;
            }
        }
        fail("MONIT trace 'provideFlowStepBegin' not found");
        return null;
    }

    /**
     * Retrieve and remove the MONIT trace 'provideFlowStepEnd' associated to the given flow step.
     * 
     * @param flowStepId
     *            The flow step identifier for which the MONIT trace is looked for.
     * @param monitLogs
     *            MONIT traces of the flow
     * @return The MONIT trace extracted matching criteria
     */
    protected LogRecord extractProviderEnd(final Object flowStepId, final List<LogRecord> monitLogs) {
        return this.extractEndOrFailure(flowStepId, TraceCode.PROVIDE_FLOW_STEP_END, monitLogs);
    }

    /**
     * Retrieve and remove the MONIT trace 'provideFlowStepFailure' associated to the given flow step.
     * 
     * @param flowStepId
     *            The flow step identifier for which the MONIT trace is looked for.
     * @param monitLogs
     *            MONIT traces of the flow
     * @return The MONIT trace extracted matching criteria
     */
    protected LogRecord extractProviderFailure(final Object flowStepId, final List<LogRecord> monitLogs) {
        return this.extractEndOrFailure(flowStepId, TraceCode.PROVIDE_FLOW_STEP_FAILURE, monitLogs);
    }

    /**
     * Retrieve and remove the MONIT trace 'consumerExtFlowStepEnd' associated to the given flow step.
     * 
     * @param flowStepId
     *            The flow step identifier for which the MONIT trace is looked for.
     * @param monitLogs
     *            MONIT traces of the flow
     * @return The MONIT trace extracted matching criteria
     */
    protected LogRecord extractConsumerExtEnd(final Object flowStepId, final List<LogRecord> monitLogs) {
        return this.extractEndOrFailure(flowStepId, TraceCode.CONSUME_EXT_FLOW_STEP_END, monitLogs);
    }

    /**
     * Retrieve and remove the MONIT trace 'consumerExtFlowStepFailure' associated to the given flow step.
     * 
     * @param flowStepId
     *            The flow step identifier for which the MONIT trace is looked for.
     * @param monitLogs
     *            MONIT traces of the flow
     * @return The MONIT trace extracted matching criteria
     */
    protected LogRecord extractConsumerExtFailure(final Object flowStepId, final List<LogRecord> monitLogs) {
        return this.extractEndOrFailure(flowStepId, TraceCode.CONSUME_EXT_FLOW_STEP_FAILURE, monitLogs);
    }

    private LogRecord extractEndOrFailure(final Object flowStepId, final TraceCode traceCode,
            final List<LogRecord> monitLogs) {
        final Iterator<LogRecord> itMonitLogs = monitLogs.iterator();
        while (itMonitLogs.hasNext()) {
            final LogRecord logRecord = itMonitLogs.next();
            final FlowLogData flowLogData = (FlowLogData) logRecord.getParameters()[0];
            if (flowLogData.get(FlowLogData.FLOW_STEP_TRACE_CODE) == traceCode
                    && flowLogData.get(FlowLogData.FLOW_STEP_ID_PROPERTY_NAME).equals(flowStepId)) {
                itMonitLogs.remove();
                return logRecord;
            }
        }
        fail("MONIT trace '" + traceCode.toString() + "' not found: flowStepId=" + flowStepId);
        return null;
    }

}
