/**
 * Copyright (c) 2014-2022 Linagora
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_IDM_ENGINE_CONFIGURATOR_CFG_FILE;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_IDM_ENGINE_CONFIGURATOR_CLASS_NAME;

import java.io.File;
import java.util.List;

import javax.management.InvalidAttributeValueException;
import javax.management.MalformedObjectNameException;
import javax.xml.parsers.DocumentBuilder;

import org.flowable.engine.IdentityService;
import org.flowable.engine.ProcessEngine;
import org.flowable.engine.ProcessEngineConfiguration;
import org.flowable.job.service.impl.asyncexecutor.AsyncExecutor;
import org.flowable.job.service.impl.asyncexecutor.DefaultAsyncJobExecutor;
import org.h2.Driver;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.ow2.petals.component.framework.AbstractComponent;
import org.ow2.petals.component.framework.jbidescriptor.generated.Component;
import org.ow2.petals.component.framework.jbidescriptor.generated.Jbi;
import org.ow2.petals.component.framework.junit.mbean.AbstractBootstrapTest;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTest;
import org.ow2.petals.flowable.identity.IdmEngineConfiguratorMock;
import org.ow2.petals.flowable.identity.file.FileIdmEngineConfigurator;
import org.ow2.petals.junit.rules.log.handler.InMemoryLogHandler;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.ebmwebsourcing.easycommons.lang.reflect.ReflectionHelper;
import com.ebmwebsourcing.easycommons.xml.DocumentBuilders;

/**
 * Unit tests of {@link FlowableSEBootstrap}
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class FlowableSEBootstrapTest extends AbstractBootstrapTest {

    @ClassRule
    public static final InMemoryLogHandler IN_MEMORY_LOG_HANDLER = new InMemoryLogHandler();

    @Rule
    public final TemporaryFolder tempFolder = new TemporaryFolder();

    public FlowableSEBootstrapTest() throws MalformedObjectNameException {
        // Constructor requires to declare the exception thrown by 'jmxClient'
    }

    /**
     * Check that the component embeds the right hard-coded default configuration (values of the component JBI
     * descriptor are set to null or empty)
     */
    @Test
    public void defaultConfiguration_definedInComponentSourceCode() throws Exception {

        // Create a minimalist JBI descriptor
        final Jbi jbiComponentConfiguration = new Jbi();
        final Component component = new Component();
        jbiComponentConfiguration.setComponent(component);
        final List<Element> params = component.getAny();

        final DocumentBuilder docBuilder = DocumentBuilders.takeDocumentBuilder();
        final Document doc = docBuilder.newDocument();
        final Element eltJdbcMaxActiveConnections = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_ACTIVE_CONNECTIONS);
        params.add(eltJdbcMaxActiveConnections);
        final Element eltJdbcMaxIdleConnections = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_IDLE_CONNECTIONS);
        params.add(eltJdbcMaxIdleConnections);
        final Element eltJdbcMaxCheckoutTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_CHECKOUT_TIME);
        params.add(eltJdbcMaxCheckoutTime);
        final Element eltJdbcMaxWaitTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_WAIT_TIME);
        params.add(eltJdbcMaxWaitTime);
        final Element eltDatabaseSchemaUpdate = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.DATABASE_SCHEMA_UPDATE);
        params.add(eltDatabaseSchemaUpdate);
        final Element eltDatabaseType = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.DATABASE_TYPE);
        params.add(eltDatabaseType);
        final Element eltJdbcUrl = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_URL);
        params.add(eltJdbcUrl);
        final Element eltJdbcUsername = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_USERNAME);
        params.add(eltJdbcUsername);
        final Element eltJdbcPassword = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_PASSWORD);
        params.add(eltJdbcPassword);

        final Element eltEnableEngineJobExecutor = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_ENABLE_JOB_EXECUTOR);
        params.add(eltEnableEngineJobExecutor);
        final Element eltEngineJobExecutorCorePoolSize = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_COREPOOLSIZE);
        params.add(eltEngineJobExecutorCorePoolSize);
        final Element eltEngineJobExecutorMaxPoolSize = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXPOOLSIZE);
        params.add(eltEngineJobExecutorMaxPoolSize);
        final Element eltEngineJobExecutorKeepAliveTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_KEEPALIVETIME);
        params.add(eltEngineJobExecutorKeepAliveTime);
        final Element eltEngineJobExecutorQueueSize = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_QUEUESIZE);
        params.add(eltEngineJobExecutorQueueSize);
        final Element eltEngineJobExecutorMaxTimerJobsPerAcquisition = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION);
        params.add(eltEngineJobExecutorMaxTimerJobsPerAcquisition);
        final Element eltEngineJobExecutorMaxAsyncJobsDuePerAcquisition = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION);
        params.add(eltEngineJobExecutorMaxAsyncJobsDuePerAcquisition);
        final Element eltEngineJobExecutorAsyncJobDueAcquireWaitTime = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME);
        params.add(eltEngineJobExecutorAsyncJobDueAcquireWaitTime);
        final Element eltEngineJobExecutorTimerJobAcquireWaitTime = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME);
        params.add(eltEngineJobExecutorTimerJobAcquireWaitTime);
        final Element eltEngineJobExecutorTimerLockTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERLOCKTIME);
        params.add(eltEngineJobExecutorTimerLockTime);
        final Element eltEngineJobExecutorAsyncJobLockTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME);
        params.add(eltEngineJobExecutorAsyncJobLockTime);

        final Element eltEnableEngineBpmnValidation = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_ENABLE_BPMN_VALIDATION);
        params.add(eltEnableEngineBpmnValidation);
        final Element eltDefaultFailedJobWaitTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_DEFAULT_FAILED_JOB_WAIT_TIME);
        params.add(eltDefaultFailedJobWaitTime);
        final Element eltAsyncFailedJobWaitTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_ASYNC_FAILED_JOB_WAIT_TIME);
        params.add(eltAsyncFailedJobWaitTime);
        final Element eltEngineIdentityServiceClassName = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CLASS_NAME);
        params.add(eltEngineIdentityServiceClassName);
        final Element eltEngineIdentityServiceCfgFile = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CFG_FILE);
        params.add(eltEngineIdentityServiceCfgFile);

        final Element eltEngineRestApiEnable = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_REST_API_ENABLE);
        params.add(eltEngineRestApiEnable);
        final Element eltEngineRestApiAddress = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_REST_API_ADDRESS);
        params.add(eltEngineRestApiAddress);
        final Element eltEngineRestApiPort = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_REST_API_PORT);
        params.add(eltEngineRestApiPort);
        final Element eltEngineRestApiAccessGroup = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_REST_API_ACCESS_PRIVILEGE);
        params.add(eltEngineRestApiAccessGroup);

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(
                this.initBootstrap(new FlowableSEBootstrap(), jbiComponentConfiguration));

        this.assertDefaultConfigurationValues("", "", "", "");
    }

    /**
     * Check that the component embeds the right hard-coded default configuration when the integer or boolean values of
     * the component JBI descriptor are set to 'space'.
     */
    @Test
    public void defaultConfiguration_ValuesSetToSpace() throws Exception {

        // Create a minimalist JBI descriptor
        final Jbi jbiComponentConfiguration = new Jbi();
        final Component component = new Component();
        jbiComponentConfiguration.setComponent(component);
        final List<Element> params = component.getAny();

        final DocumentBuilder docBuilder = DocumentBuilders.takeDocumentBuilder();
        final Document doc = docBuilder.newDocument();
        final Element eltJdbcMaxActiveConnections = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_ACTIVE_CONNECTIONS);
        eltJdbcMaxActiveConnections.setTextContent(" ");
        params.add(eltJdbcMaxActiveConnections);
        final Element eltJdbcMaxIdleConnections = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_IDLE_CONNECTIONS);
        eltJdbcMaxIdleConnections.setTextContent(" ");
        params.add(eltJdbcMaxIdleConnections);
        final Element eltJdbcMaxCheckoutTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_CHECKOUT_TIME);
        eltJdbcMaxCheckoutTime.setTextContent(" ");
        params.add(eltJdbcMaxCheckoutTime);
        final Element eltJdbcMaxWaitTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_WAIT_TIME);
        eltJdbcMaxWaitTime.setTextContent(" ");
        params.add(eltJdbcMaxWaitTime);
        final Element eltDatabaseType = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.DATABASE_TYPE);
        eltDatabaseType.setTextContent(" ");
        params.add(eltDatabaseType);
        final Element eltDatabaseSchemaUpdate = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.DATABASE_SCHEMA_UPDATE);
        eltDatabaseSchemaUpdate.setTextContent(" ");
        params.add(eltDatabaseSchemaUpdate);
        final Element eltJdbcUrl = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_URL);
        eltJdbcUrl.setTextContent(" ");
        params.add(eltJdbcUrl);
        final Element eltJdbcUsername = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_USERNAME);
        eltJdbcUsername.setTextContent(" ");
        params.add(eltJdbcUsername);
        final Element eltJdbcPassword = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_PASSWORD);
        eltJdbcPassword.setTextContent(" ");
        params.add(eltJdbcPassword);

        final Element eltEnableEngineJobExecutor = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_ENABLE_JOB_EXECUTOR);
        eltEnableEngineJobExecutor.setTextContent(" ");
        params.add(eltEnableEngineJobExecutor);
        final Element eltEngineJobExecutorCorePoolSize = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_COREPOOLSIZE);
        eltEngineJobExecutorCorePoolSize.setTextContent(" ");
        params.add(eltEngineJobExecutorCorePoolSize);
        final Element eltEngineJobExecutorMaxPoolSize = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXPOOLSIZE);
        eltEngineJobExecutorMaxPoolSize.setTextContent(" ");
        params.add(eltEngineJobExecutorMaxPoolSize);
        final Element eltEngineJobExecutorKeepAliveTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_KEEPALIVETIME);
        eltEngineJobExecutorKeepAliveTime.setTextContent(" ");
        params.add(eltEngineJobExecutorKeepAliveTime);
        final Element eltEngineJobExecutorQueueSize = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_QUEUESIZE);
        eltEngineJobExecutorQueueSize.setTextContent(" ");
        params.add(eltEngineJobExecutorQueueSize);
        final Element eltEngineJobExecutorMaxTimerJobsPerAcquisition = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION);
        eltEngineJobExecutorMaxTimerJobsPerAcquisition.setTextContent(" ");
        params.add(eltEngineJobExecutorMaxTimerJobsPerAcquisition);
        final Element eltEngineJobExecutorMaxAsyncJobsDuePerAcquisition = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION);
        eltEngineJobExecutorMaxAsyncJobsDuePerAcquisition.setTextContent(" ");
        params.add(eltEngineJobExecutorMaxAsyncJobsDuePerAcquisition);
        final Element eltEngineJobExecutorAsyncJobDueAcquireWaitTime = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME);
        eltEngineJobExecutorAsyncJobDueAcquireWaitTime.setTextContent(" ");
        params.add(eltEngineJobExecutorAsyncJobDueAcquireWaitTime);
        final Element eltEngineJobExecutorTimerJobAcquireWaitTime = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME);
        eltEngineJobExecutorTimerJobAcquireWaitTime.setTextContent(" ");
        params.add(eltEngineJobExecutorTimerJobAcquireWaitTime);
        final Element eltEngineJobExecutorTimerLockTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERLOCKTIME);
        eltEngineJobExecutorTimerLockTime.setTextContent(" ");
        params.add(eltEngineJobExecutorTimerLockTime);
        final Element eltEngineJobExecutorAsyncJobLockTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME);
        eltEngineJobExecutorAsyncJobLockTime.setTextContent(" ");
        params.add(eltEngineJobExecutorAsyncJobLockTime);

        final Element eltEnableEngineBpmnValidation = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_ENABLE_BPMN_VALIDATION);
        eltEnableEngineBpmnValidation.setTextContent(" ");
        params.add(eltEnableEngineBpmnValidation);
        final Element eltDefaultFailedJobWaitTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_DEFAULT_FAILED_JOB_WAIT_TIME);
        eltDefaultFailedJobWaitTime.setTextContent(" ");
        params.add(eltDefaultFailedJobWaitTime);
        final Element eltAsyncFailedJobWaitTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_ASYNC_FAILED_JOB_WAIT_TIME);
        eltAsyncFailedJobWaitTime.setTextContent(" ");
        params.add(eltAsyncFailedJobWaitTime);
        final Element eltEngineIdentityServiceClassName = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CLASS_NAME);
        eltEngineIdentityServiceClassName.setTextContent(" ");
        params.add(eltEngineIdentityServiceClassName);
        final Element eltEngineIdentityServiceCfgFile = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CFG_FILE);
        eltEngineIdentityServiceCfgFile.setTextContent(" ");
        params.add(eltEngineIdentityServiceCfgFile);

        final Element eltEngineRestApiEnable = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_REST_API_ENABLE);
        eltEngineRestApiEnable.setTextContent(" ");
        params.add(eltEngineRestApiEnable);
        final Element eltEngineRestApiPort = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_REST_API_PORT);
        eltEngineRestApiPort.setTextContent(" ");
        params.add(eltEngineRestApiPort);

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(
                this.initBootstrap(new FlowableSEBootstrap(), jbiComponentConfiguration));

        this.assertDefaultConfigurationValues(" ", " ", " ", " ");
    }

    /**
     * Check that the component embeds the right hard-coded default configuration when the integer or boolean values of
     * the component JBI descriptor are set to invalid values.
     */
    @Test
    public void defaultConfiguration_InvalidValues() throws Exception {

        // Create a minimalist JBI descriptor
        final Jbi jbiComponentConfiguration = new Jbi();
        final Component component = new Component();
        jbiComponentConfiguration.setComponent(component);
        final List<Element> params = component.getAny();

        final DocumentBuilder docBuilder = DocumentBuilders.takeDocumentBuilder();
        final Document doc = docBuilder.newDocument();
        final Element eltJdbcMaxActiveConnections = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_ACTIVE_CONNECTIONS);
        eltJdbcMaxActiveConnections.setTextContent("invalid-value");
        params.add(eltJdbcMaxActiveConnections);
        final Element eltJdbcMaxIdleConnections = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_IDLE_CONNECTIONS);
        eltJdbcMaxIdleConnections.setTextContent("invalid-value");
        params.add(eltJdbcMaxIdleConnections);
        final Element eltJdbcMaxCheckoutTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_CHECKOUT_TIME);
        eltJdbcMaxCheckoutTime.setTextContent("invalid-value");
        params.add(eltJdbcMaxCheckoutTime);
        final Element eltJdbcMaxWaitTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_WAIT_TIME);
        eltJdbcMaxWaitTime.setTextContent("invalid-value");
        params.add(eltJdbcMaxWaitTime);
        final Element eltDatabaseSchemaUpdate = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.DATABASE_SCHEMA_UPDATE);
        eltDatabaseSchemaUpdate.setTextContent("invalid-value");
        params.add(eltDatabaseSchemaUpdate);

        final Element eltEnableEngineJobExecutor = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_ENABLE_JOB_EXECUTOR);
        eltEnableEngineJobExecutor.setTextContent("invalid-value");
        params.add(eltEnableEngineJobExecutor);
        final Element eltEngineJobExecutorCorePoolSize = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_COREPOOLSIZE);
        eltEngineJobExecutorCorePoolSize.setTextContent("invalid-value");
        params.add(eltEngineJobExecutorCorePoolSize);
        final Element eltEngineJobExecutorMaxPoolSize = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXPOOLSIZE);
        eltEngineJobExecutorMaxPoolSize.setTextContent("invalid-value");
        params.add(eltEngineJobExecutorMaxPoolSize);
        final Element eltEngineJobExecutorKeepAliveTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_KEEPALIVETIME);
        eltEngineJobExecutorKeepAliveTime.setTextContent("invalid-value");
        params.add(eltEngineJobExecutorKeepAliveTime);
        final Element eltEngineJobExecutorQueueSize = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_QUEUESIZE);
        eltEngineJobExecutorQueueSize.setTextContent("invalid-value");
        params.add(eltEngineJobExecutorQueueSize);
        final Element eltEngineJobExecutorMaxTimerJobsPerAcquisition = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION);
        eltEngineJobExecutorMaxTimerJobsPerAcquisition.setTextContent("invalid-value");
        params.add(eltEngineJobExecutorMaxTimerJobsPerAcquisition);
        final Element eltEngineJobExecutorMaxAsyncJobsDuePerAcquisition = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION);
        eltEngineJobExecutorMaxAsyncJobsDuePerAcquisition.setTextContent("invalid-value");
        params.add(eltEngineJobExecutorMaxAsyncJobsDuePerAcquisition);
        final Element eltEngineJobExecutorAsyncJobDueAcquireWaitTime = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME);
        eltEngineJobExecutorAsyncJobDueAcquireWaitTime.setTextContent("invalid-value");
        params.add(eltEngineJobExecutorAsyncJobDueAcquireWaitTime);
        final Element eltEngineJobExecutorTimerJobAcquireWaitTime = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME);
        eltEngineJobExecutorTimerJobAcquireWaitTime.setTextContent("invalid-value");
        params.add(eltEngineJobExecutorTimerJobAcquireWaitTime);
        final Element eltEngineJobExecutorTimerLockTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERLOCKTIME);
        eltEngineJobExecutorTimerLockTime.setTextContent("invalid-value");
        params.add(eltEngineJobExecutorTimerLockTime);
        final Element eltEngineJobExecutorAsyncJobLockTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME);
        eltEngineJobExecutorAsyncJobLockTime.setTextContent("invalid-value");
        params.add(eltEngineJobExecutorAsyncJobLockTime);

        final Element eltEnableEngineBpmnValidation = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_ENABLE_BPMN_VALIDATION);
        eltEnableEngineBpmnValidation.setTextContent("invalid-value");
        params.add(eltEnableEngineBpmnValidation);
        final Element eltDefaultFailedJobWaitTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_DEFAULT_FAILED_JOB_WAIT_TIME);
        eltDefaultFailedJobWaitTime.setTextContent("invalid-value");
        params.add(eltDefaultFailedJobWaitTime);
        final Element eltAsyncFailedJobWaitTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_ASYNC_FAILED_JOB_WAIT_TIME);
        eltAsyncFailedJobWaitTime.setTextContent("invalid-value");
        params.add(eltAsyncFailedJobWaitTime);
        final Element eltEngineIdentityServiceClassName = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CLASS_NAME);
        eltEngineIdentityServiceClassName.setTextContent("invalid-value");
        params.add(eltEngineIdentityServiceClassName);
        final Element eltEngineIdentityServiceCfgFile = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CFG_FILE);
        eltEngineIdentityServiceCfgFile.setTextContent("invalid-value");
        params.add(eltEngineIdentityServiceCfgFile);

        final Element eltEngineRestApiEnable = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_REST_API_ENABLE);
        eltEngineRestApiEnable.setTextContent("invalid-value");
        params.add(eltEngineRestApiEnable);
        final Element eltEngineRestApiPort = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_REST_API_PORT);
        eltEngineRestApiPort.setTextContent("invalid-value");
        params.add(eltEngineRestApiPort);

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(
                this.initBootstrap(new FlowableSEBootstrap(), jbiComponentConfiguration));

        this.assertDefaultConfigurationValues(null, null, null, null);
    }

    /**
     * Check that the component bootstrap get the given configuration when configuration parameters are set to valid
     * values into the component JBI descriptor.
     */
    @Test
    public void defaultConfiguration_ValidValues() throws Exception {

        // Create a minimalist JBI descriptor
        final Jbi jbiComponentConfiguration = new Jbi();
        final Component component = new Component();
        jbiComponentConfiguration.setComponent(component);
        final List<Element> params = component.getAny();

        final DocumentBuilder docBuilder = DocumentBuilders.takeDocumentBuilder();
        final Document doc = docBuilder.newDocument();

        final Element eltJdbcMaxActiveConnections = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_ACTIVE_CONNECTIONS);
        final int jdbcMaxActiveConnections = 15;
        eltJdbcMaxActiveConnections.setTextContent(String.valueOf(jdbcMaxActiveConnections));
        params.add(eltJdbcMaxActiveConnections);

        final Element eltJdbcMaxIdleConnections = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_IDLE_CONNECTIONS);
        final int jdbcMaxIdleConnections = 4;
        eltJdbcMaxIdleConnections.setTextContent(String.valueOf(jdbcMaxIdleConnections));
        params.add(eltJdbcMaxIdleConnections);

        final Element eltJdbcMaxCheckoutTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_CHECKOUT_TIME);
        final int jdbcMaxCheckoutTime = 25000;
        eltJdbcMaxCheckoutTime.setTextContent(String.valueOf(jdbcMaxCheckoutTime));
        params.add(eltJdbcMaxCheckoutTime);

        final Element eltJdbcMaxWaitTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_MAX_WAIT_TIME);
        final int jdbcMaxWaitTime = 15000;
        eltJdbcMaxWaitTime.setTextContent(String.valueOf(jdbcMaxWaitTime));
        params.add(eltJdbcMaxWaitTime);

        final Element eltDatabaseSchemaUpdate = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.DATABASE_SCHEMA_UPDATE);
        final String databaseSchemaUpdate = "create-drop";
        eltDatabaseSchemaUpdate.setTextContent(databaseSchemaUpdate);
        params.add(eltDatabaseSchemaUpdate);

        final Element eltDatabaseType = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.DATABASE_TYPE);
        final String databaseType = "postgres";
        eltDatabaseType.setTextContent(databaseType);
        params.add(eltDatabaseType);

        final Element eltJdbcUrl = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_URL);
        final String jdbcUrl = this.tempFolder.newFile(FlowableSEConstants.DBServer.DEFAULT_JDBC_URL_DATABASE_FILENAME)
                .getAbsolutePath();
        eltJdbcUrl.setTextContent(jdbcUrl);
        params.add(eltJdbcUrl);

        final Element eltJdbcUsername = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_USERNAME);
        final String jdbcUsername = "sa";
        eltJdbcUsername.setTextContent(jdbcUsername);
        params.add(eltJdbcUsername);

        final Element eltJdbcPassword = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.DBServer.JDBC_PASSWORD);
        final String jdbcPassword = "sa.password";
        eltJdbcPassword.setTextContent(jdbcPassword);
        params.add(eltJdbcPassword);

        final Element eltEnableEngineJobExecutor = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_ENABLE_JOB_EXECUTOR);
        final String engineEnableJobExecutor = Boolean.FALSE.toString();
        eltEnableEngineJobExecutor.setTextContent(engineEnableJobExecutor);
        params.add(eltEnableEngineJobExecutor);

        final Element eltEngineJobExecutorCorePoolSize = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_COREPOOLSIZE);
        final int jobExecutorCorePoolSize = 10;
        eltEngineJobExecutorCorePoolSize.setTextContent(String.valueOf(jobExecutorCorePoolSize));
        params.add(eltEngineJobExecutorCorePoolSize);

        final Element eltEngineJobExecutorMaxPoolSize = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXPOOLSIZE);
        final int jobExecutorMaxPoolSize = 100;
        eltEngineJobExecutorMaxPoolSize.setTextContent(String.valueOf(jobExecutorMaxPoolSize));
        params.add(eltEngineJobExecutorMaxPoolSize);

        final Element eltEngineJobExecutorKeepAliveTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_KEEPALIVETIME);
        final int jobExecutorKeepAliveTime = 105000;
        eltEngineJobExecutorKeepAliveTime.setTextContent(String.valueOf(jobExecutorKeepAliveTime));
        params.add(eltEngineJobExecutorKeepAliveTime);

        final Element eltEngineJobExecutorQueueSize = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_QUEUESIZE);
        final int jobExecutorQueueSize = 1050;
        eltEngineJobExecutorQueueSize.setTextContent(String.valueOf(jobExecutorQueueSize));
        params.add(eltEngineJobExecutorQueueSize);

        final Element eltEngineJobExecutorMaxTimerJobsPerAcquisition = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION);
        final int jobExecutorMaxTimerJobsPerAcquisition = 50;
        eltEngineJobExecutorMaxTimerJobsPerAcquisition
                .setTextContent(String.valueOf(jobExecutorMaxTimerJobsPerAcquisition));
        params.add(eltEngineJobExecutorMaxTimerJobsPerAcquisition);

        final Element eltEngineJobExecutorMaxAsyncJobsDuePerAcquisition = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION);
        final int jobExecutorMaxAsyncJobsDuePerAcquisition = 50;
        eltEngineJobExecutorMaxAsyncJobsDuePerAcquisition
                .setTextContent(String.valueOf(jobExecutorMaxAsyncJobsDuePerAcquisition));
        params.add(eltEngineJobExecutorMaxAsyncJobsDuePerAcquisition);

        final Element eltEngineJobExecutorAsyncJobDueAcquireWaitTime = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME);
        final int jobExecutorAsyncJobDueAcquireWaitTime = 50000;
        eltEngineJobExecutorAsyncJobDueAcquireWaitTime
                .setTextContent(String.valueOf(jobExecutorAsyncJobDueAcquireWaitTime));
        params.add(eltEngineJobExecutorAsyncJobDueAcquireWaitTime);

        final Element eltEngineJobExecutorTimerJobAcquireWaitTime = doc.createElementNS(
                FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME);
        final int jobExecutorTimerJobAcquireWaitTime = 50000;
        eltEngineJobExecutorTimerJobAcquireWaitTime.setTextContent(String.valueOf(jobExecutorTimerJobAcquireWaitTime));
        params.add(eltEngineJobExecutorTimerJobAcquireWaitTime);

        final Element eltEngineJobExecutorTimerLockTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERLOCKTIME);
        final int jobExecutorTimerLockTime = 50000;
        eltEngineJobExecutorTimerLockTime.setTextContent(String.valueOf(jobExecutorTimerLockTime));
        params.add(eltEngineJobExecutorTimerLockTime);

        final Element eltEngineJobExecutorAsyncJobLockTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME);
        final int jobExecutorAsyncJobLockTime = 50000;
        eltEngineJobExecutorAsyncJobLockTime.setTextContent(String.valueOf(jobExecutorAsyncJobLockTime));
        params.add(eltEngineJobExecutorAsyncJobLockTime);

        final Element eltEnableEngineBpmnValidation = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_ENABLE_BPMN_VALIDATION);
        final String engineEnableBpmnValidation = Boolean.FALSE.toString();
        eltEnableEngineBpmnValidation.setTextContent(engineEnableBpmnValidation);
        params.add(eltEnableEngineBpmnValidation);

        final Element eltDefaultFailedJobWaitTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_DEFAULT_FAILED_JOB_WAIT_TIME);
        final int defaultFailedJobWaitTime = 5;
        eltDefaultFailedJobWaitTime.setTextContent(String.valueOf(defaultFailedJobWaitTime));
        params.add(eltDefaultFailedJobWaitTime);

        final Element eltAsyncFailedJobWaitTime = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_ASYNC_FAILED_JOB_WAIT_TIME);
        final int asyncFailedJobWaitTime = 15;
        eltAsyncFailedJobWaitTime.setTextContent(String.valueOf(asyncFailedJobWaitTime));
        params.add(eltAsyncFailedJobWaitTime);

        final Element eltIdmEngineConfiguratorClassName = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CLASS_NAME);
        final String idmEngineConfiguratorClassName = IdmEngineConfiguratorMock.class.getName();
        eltIdmEngineConfiguratorClassName.setTextContent(idmEngineConfiguratorClassName);
        params.add(eltIdmEngineConfiguratorClassName);

        final Element eltIdmEngineConfiguratorCfgFile = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CFG_FILE);
        final String idmEngineConfiguratorCfgFile = "my.idm-engine-configurator.cfg.file";
        eltIdmEngineConfiguratorCfgFile.setTextContent(idmEngineConfiguratorCfgFile);
        params.add(eltIdmEngineConfiguratorCfgFile);

        final Element eltEngineRestApiEnable = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_REST_API_ENABLE);
        final boolean engineRestApiEnable = false;
        eltEngineRestApiEnable.setTextContent(Boolean.toString(engineRestApiEnable));
        params.add(eltEngineRestApiEnable);

        final Element eltEngineRestApiAddress = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_REST_API_ADDRESS);
        final String engineRestApiAddress = "localhost";
        eltEngineRestApiAddress.setTextContent(engineRestApiAddress);
        params.add(eltEngineRestApiAddress);

        final Element eltEngineRestApiPort = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_REST_API_PORT);
        final int engineRestApiPort = 12345;
        eltEngineRestApiPort.setTextContent(Integer.toString(engineRestApiPort));
        params.add(eltEngineRestApiPort);

        final Element eltEngineRestApiAccessGroup = doc.createElementNS(FlowableSEConstants.NAMESPACE_COMP,
                FlowableSEConstants.ENGINE_REST_API_ACCESS_PRIVILEGE);
        final String engineRestApiAccessGroup = "test-group";
        eltEngineRestApiAccessGroup.setTextContent(engineRestApiAccessGroup);
        params.add(eltEngineRestApiAccessGroup);

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(
                this.initBootstrap(new FlowableSEBootstrap(), jbiComponentConfiguration));

        assertEquals(FlowableSEConstants.DBServer.DEFAULT_JDBC_DRIVER,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_DRIVER));
        assertEquals(jdbcUrl, this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_URL));
        assertEquals(jdbcUsername,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_USERNAME));
        assertEquals(jdbcPassword,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_PASSWORD));
        assertEquals(jdbcMaxActiveConnections,
                this.jmxClient.getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_JDBC_MAX_ACTIVE_CONNECTIONS));
        assertEquals(jdbcMaxIdleConnections,
                this.jmxClient.getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_JDBC_MAX_IDLE_CONNECTIONS));
        assertEquals(jdbcMaxCheckoutTime,
                this.jmxClient.getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_JDBC_MAX_CHECKOUT_TIME));
        assertEquals(jdbcMaxWaitTime,
                this.jmxClient.getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_JDBC_MAX_WAIT_TIME));
        assertEquals(databaseType,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_DATABASE_TYPE));
        assertEquals(databaseSchemaUpdate,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_DATABASE_SCHEMA_UPDATE));
        assertEquals(engineEnableBpmnValidation, this.jmxClient
                .getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_ENGINE_ENABLE_BPMN_VALIDATION));
        assertEquals(defaultFailedJobWaitTime, this.jmxClient
                .getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_DEFAULT_FAILED_JOB_WAITTIME));
        assertEquals(asyncFailedJobWaitTime, this.jmxClient
                .getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_ASYNC_FAILED_JOB_WAITTIME));
        assertEquals(idmEngineConfiguratorClassName, this.jmxClient
                .getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CLASS_NAME));
        assertEquals(DEFAULT_IDM_ENGINE_CONFIGURATOR_CFG_FILE, this.jmxClient
                .getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CFG_FILE));
        assertEquals(engineEnableJobExecutor,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_ENGINE_ENABLE_JOB_EXECUTOR));
        assertEquals(jobExecutorCorePoolSize, this.jmxClient
                .getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_COREPOOLSIZE));
        assertEquals(jobExecutorMaxPoolSize, this.jmxClient
                .getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE));
        assertEquals(jobExecutorKeepAliveTime, this.jmxClient
                .getBootstrapAttributeAsLong(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_KEEPALIVETIME));
        assertEquals(jobExecutorQueueSize,
                this.jmxClient.getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_QUEUESIZE));
        assertEquals(engineRestApiEnable,
                this.jmxClient.getBootstrapAttributeAsBoolean(FlowableSEBootstrap.ATTR_NAME_ENGINE_REST_API_ENABLE));
        assertEquals(engineRestApiAddress,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_ENGINE_REST_API_ADDRESS));
        assertEquals(engineRestApiPort,
                this.jmxClient.getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_REST_API_PORT));
        assertEquals(engineRestApiAccessGroup, this.jmxClient
                .getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_ENGINE_REST_API_ACCESS_PRIVILEGE));

        // TODO: What to do with this config parameter not available at Mbean level ?
        // assertEquals(jobExecutorMaxTimerJobsPerAcquisition,
        // bootstrap.getEngineJobExecutorMaxTimerJobsPerAcquisition());
        // assertEquals(jobExecutorMaxAsyncJobsDuePerAcquisition,
        // bootstrap.getEngineJobExecutorMaxAsyncJobsDuePerAcquisition());
        // assertEquals(jobExecutorAsyncJobDueAcquireWaitTime,
        // bootstrap.getEngineJobExecutorAsyncJobDueAcquireWaitTime());
        // assertEquals(jobExecutorTimerJobAcquireWaitTime, bootstrap.getEngineJobExecutorTimerJobAcquireWaitTime());
        // assertEquals(jobExecutorTimerLockTime, bootstrap.getEngineJobExecutorTimerLockTime());
        // assertEquals(jobExecutorAsyncJobLockTime, bootstrap.getEngineJobExecutorAsyncJobLockTime());
    }

    /**
     * Check that the component embeds the right default configuration in its JBI descriptor (values set to their
     * default value in jbi.xml)
     */
    @Test
    public void defaultConfiguration_definedInJbiDescriptor() throws Exception {

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(this.initBootstrap(new FlowableSEBootstrap()));

        this.assertDefaultConfigurationValues("", "sa", "", "");
    }

    private void assertDefaultConfigurationValues(final String expectedJdbcUrl, final String expectedJdbcUsername,
            final String expectedJdbcPassword, final String expectedDatabaseType) throws Exception {

        assertEquals(FlowableSEConstants.DBServer.DEFAULT_JDBC_DRIVER,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_DRIVER));
        assertEquals(expectedJdbcUrl,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_URL));
        assertEquals(expectedJdbcUsername,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_USERNAME));
        assertEquals(expectedJdbcPassword,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_PASSWORD));
        assertEquals(FlowableSEConstants.DBServer.DEFAULT_JDBC_MAX_ACTIVE_CONNECTIONS,
                this.jmxClient.getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_JDBC_MAX_ACTIVE_CONNECTIONS));
        assertEquals(FlowableSEConstants.DBServer.DEFAULT_JDBC_MAX_IDLE_CONNECTIONS,
                this.jmxClient.getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_JDBC_MAX_IDLE_CONNECTIONS));
        assertEquals(FlowableSEConstants.DBServer.DEFAULT_JDBC_MAX_CHECKOUT_TIME,
                this.jmxClient.getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_JDBC_MAX_CHECKOUT_TIME));
        assertEquals(FlowableSEConstants.DBServer.DEFAULT_JDBC_MAX_WAIT_TIME,
                this.jmxClient.getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_JDBC_MAX_WAIT_TIME));
        assertEquals(expectedDatabaseType,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_DATABASE_TYPE));
        assertEquals(FlowableSEConstants.DBServer.DEFAULT_DATABASE_SCHEMA_UPDATE,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_DATABASE_SCHEMA_UPDATE));
        assertEquals(String.valueOf(FlowableSEConstants.DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION), this.jmxClient
                .getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_ENGINE_ENABLE_BPMN_VALIDATION));
        assertEquals(FlowableSEConstants.DEFAULT_DEFAULT_FAILED_JOB_WAIT_TIME, this.jmxClient
                .getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_DEFAULT_FAILED_JOB_WAITTIME));
        assertEquals(FlowableSEConstants.DEFAULT_ASYNC_FAILED_JOB_WAIT_TIME, this.jmxClient
                .getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_ASYNC_FAILED_JOB_WAITTIME));
        assertEquals(DEFAULT_IDM_ENGINE_CONFIGURATOR_CLASS_NAME, this.jmxClient
                .getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CLASS_NAME));
        assertEquals(DEFAULT_IDM_ENGINE_CONFIGURATOR_CFG_FILE, this.jmxClient
                .getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CFG_FILE));
        assertEquals(String.valueOf(FlowableSEConstants.DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR),
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_ENGINE_ENABLE_JOB_EXECUTOR));
        assertEquals(FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_COREPOOLSIZE, this.jmxClient
                .getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_COREPOOLSIZE));
        assertEquals(FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE, this.jmxClient
                .getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE));
        assertEquals(FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_KEEPALIVETIME, this.jmxClient
                .getBootstrapAttributeAsLong(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_KEEPALIVETIME));
        assertEquals(FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_QUEUESIZE,
                this.jmxClient.getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_QUEUESIZE));
        assertEquals(FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION,
                this.jmxClient.getBootstrapAttributeAsInt(
                        FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION));
        assertEquals(FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION,
                this.jmxClient.getBootstrapAttributeAsInt(
                        FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION));
        assertEquals(FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME, this.jmxClient
                .getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME));
        assertEquals(FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME, this.jmxClient
                .getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME));
        assertEquals(FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_TIMERLOCKTIME, this.jmxClient
                .getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_TIMERLOCKTIME));
        assertEquals(FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME, this.jmxClient
                .getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME));
        assertEquals(FlowableSEConstants.DEFAULT_ENGINE_REST_API_ENABLE,
                this.jmxClient.getBootstrapAttributeAsBoolean(FlowableSEBootstrap.ATTR_NAME_ENGINE_REST_API_ENABLE));
        assertEquals(FlowableSEConstants.DEFAULT_ENGINE_REST_API_ADDRESS,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_ENGINE_REST_API_ADDRESS));
        assertEquals(FlowableSEConstants.DEFAULT_ENGINE_REST_API_PORT,
                this.jmxClient.getBootstrapAttributeAsInt(FlowableSEBootstrap.ATTR_NAME_ENGINE_REST_API_PORT));
        assertEquals(FlowableSEConstants.DEFAULT_ENGINE_REST_API_ACCESS_PRIVILEGE, this.jmxClient
                .getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_ENGINE_REST_API_ACCESS_PRIVILEGE));
    }

    /**
     * Check to set an invalid URL as JDBC URL
     */
    @Test(expected = InvalidAttributeValueException.class)
    public void setJdbcUrl_InvalidURL() throws Exception {

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(this.initBootstrap(new FlowableSEBootstrap()));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_URL, "// invalid-url/foo:http+ftp");
    }

    /**
     * Check to set a valid URL as JDBC URL
     */
    @Test
    public void setJdbcUrl_ValidURL() throws Exception {

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(this.initBootstrap(new FlowableSEBootstrap()));

        // A simple URL
        final String expectedSimpleUrl = "http://path";
        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_URL, expectedSimpleUrl);
        assertEquals(expectedSimpleUrl,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_URL));

        // A JDBC URL that is not an URL
        final String expectedUrlNotUrl = "jdbc:h2:tcp://localhost/mem:flowable";
        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_URL, expectedUrlNotUrl);
        assertEquals(expectedUrlNotUrl,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_URL));

    }

    /**
     * Check to set the value 'space' as JDBC URL
     */
    @Test
    public void setJdbcUrl_SpaceURL() throws Exception {

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(this.initBootstrap(new FlowableSEBootstrap()));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_URL, null);
        assertEquals("", this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_URL));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_URL, "");
        assertEquals("", this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_URL));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_URL, " ");
        assertEquals("", this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_URL));
    }

    /**
     * Check to set an unknown class as JDBC driver
     */
    @Test(expected = InvalidAttributeValueException.class)
    public void setJdbcDriver_Unknown() throws Exception {

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(this.initBootstrap(new FlowableSEBootstrap()));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_DRIVER, "unknown.class");
    }

    /**
     * Check to set a known class as JDBC Driver
     */
    @Test
    public void setJdbcDriver_ValidDriver() throws Exception {

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(this.initBootstrap(new FlowableSEBootstrap()));

        final String expectedDriver = Driver.class.getName();
        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_DRIVER, expectedDriver);
        assertEquals(expectedDriver,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_DRIVER));
    }

    /**
     * <p>
     * Check to set the value 'space' or <code>null</code> as JDBC Driver.
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the value is successfully set,</li>
     * <li>the default value of the parameter is retrieved next.</li>
     * </ul>
     */
    @Test
    public void setJdbcDriver_SpaceValue() throws Exception {

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(this.initBootstrap(new FlowableSEBootstrap()));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_DRIVER, null);
        assertEquals(FlowableSEConstants.DBServer.DEFAULT_JDBC_DRIVER,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_DRIVER));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_DRIVER, "");
        assertEquals(FlowableSEConstants.DBServer.DEFAULT_JDBC_DRIVER,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_DRIVER));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_DRIVER, " ");
        assertEquals(FlowableSEConstants.DBServer.DEFAULT_JDBC_DRIVER,
                this.jmxClient.getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_JDBC_DRIVER));
    }

    /**
     * <p>
     * Check to set the value 'space' or <code>null</code> as identity service class name.
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>the value is successfully set,</li>
     * <li>the default value of the parameter is retrieved next.</li>
     * </ul>
     */
    @Test
    public void setEngineIdentityServiceClassName_SpaceValue() throws Exception {

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(this.initBootstrap(new FlowableSEBootstrap()));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CLASS_NAME, null);
        assertEquals(DEFAULT_IDM_ENGINE_CONFIGURATOR_CLASS_NAME, this.jmxClient
                .getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CLASS_NAME));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CLASS_NAME, "");
        assertEquals(DEFAULT_IDM_ENGINE_CONFIGURATOR_CLASS_NAME, this.jmxClient
                .getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CLASS_NAME));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CLASS_NAME, " ");
        assertEquals(DEFAULT_IDM_ENGINE_CONFIGURATOR_CLASS_NAME, this.jmxClient
                .getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CLASS_NAME));
    }

    /**
     * Check to set a not loadable class as identity service class name
     */
    @Test(expected = InvalidAttributeValueException.class)
    public void setEngineIdentityServiceCfgFile_ClassNotLoadable() throws Exception {

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(this.initBootstrap(new FlowableSEBootstrap()));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CLASS_NAME,
                "not-loadable-class");
    }

    /**
     * Check to set a class not implementing {@link IdentityService} as identity service class name
     */
    @Test(expected = InvalidAttributeValueException.class)
    public void setEngineIdentityServiceCfgFile_ClassNotImplementingIdentityService() throws Exception {

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(this.initBootstrap(new FlowableSEBootstrap()));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CLASS_NAME,
                String.class.getName());
    }

    /**
     * Check to set the value 'space' or <code>null</code> as identity service configuration file
     */
    @Test
    public void setEngineIdentityServiceCfgFile_SpaceValue() throws Exception {

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(this.initBootstrap(new FlowableSEBootstrap()));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CFG_FILE, null);
        assertEquals(null, this.jmxClient
                .getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CFG_FILE));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CFG_FILE, "");
        assertEquals(null, this.jmxClient
                .getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CFG_FILE));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CFG_FILE, " ");
        assertEquals(null, this.jmxClient
                .getBootstrapAttributeAsString(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CFG_FILE));
    }

    /**
     * Check to set an inexisting absolute file as identity service configuration file
     */
    @Test(expected = InvalidAttributeValueException.class)
    public void setEngineIdentityServiceCfgFile_InexistingFile() throws Exception {

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(this.initBootstrap(new FlowableSEBootstrap()));

        // Set an unexisting absolute file
        final File absFile = this.tempFolder.newFile();
        assertTrue(absFile.delete()); // We must remove the file previously created

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CFG_FILE,
                absFile.getAbsolutePath());
    }

    /**
     * Check to set an invalid tcp port for the REST API (max is 65535)
     */
    @Test(expected = InvalidAttributeValueException.class)
    public void setEngineRestApiPort_InvalidPort() throws Exception {

        this.embeddedJmxSrvCon.registerConfigurationInstallerMBean(this.initBootstrap(new FlowableSEBootstrap()));

        this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_ENGINE_REST_API_PORT, 66666);
    }

    /**
     * Check that valid values set through the component bootstrap through JMX are correctly used by component.
     */
    @Test
    public void setValuesAreUsed() throws Throwable {
        // To access the component bootstrap through JMX, the component has not to be installed, only loaded
        final ComponentUnderTest componentUnderTest = new ComponentUnderTest(false, false)
                .addLogHandler(IN_MEMORY_LOG_HANDLER.getHandler()).addEmbeddedJmxSrv(this.embeddedJmxSrvCon);
        componentUnderTest.create();

        try {
            final AbstractComponent component = componentUnderTest.getComponentObject();
            assertNotNull(component);
            assertTrue(component instanceof FlowableSE);
            final FlowableSE flowableComponent = (FlowableSE) component;

            // Set values using the component bootstrap through JMX
            final String databaseType = "h2";
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_DATABASE_TYPE, databaseType);
            final String databaseSchemaUpdate = "create-drop";
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_DATABASE_SCHEMA_UPDATE,
                    databaseSchemaUpdate);

            final String jdbcDriver = Driver.class.getName();
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_DRIVER, jdbcDriver);
            final String jdbcUsername = "my-user";
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_USERNAME, jdbcUsername);
            final String jdbcPassword = "my-user-pwd";
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_PASSWORD, jdbcPassword);
            final File databaseFile = this.tempFolder.newFile("my-database.db");
            final String jdbcUrl = String.format("jdbc:h2:%s", databaseFile.toURI().toURL().toExternalForm());
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_URL, jdbcUrl);
            final int jdbcMaxActiveConnections = 12;
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_MAX_ACTIVE_CONNECTIONS,
                    Integer.valueOf(jdbcMaxActiveConnections));
            final int jdbcMaxIdleConnections = 21;
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_MAX_IDLE_CONNECTIONS,
                    Integer.valueOf(jdbcMaxIdleConnections));
            final int jdbcMaxCheckoutTime = 1592;
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_MAX_CHECKOUT_TIME,
                    Integer.valueOf(jdbcMaxCheckoutTime));
            final int jdbcMaxWaitTime = 56842;
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_JDBC_MAX_WAIT_TIME,
                    Integer.valueOf(jdbcMaxWaitTime));

            final boolean jobExecutorEnableJobExecutor = true;
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_ENGINE_ENABLE_JOB_EXECUTOR,
                    String.valueOf(jobExecutorEnableJobExecutor));

            final int jobExecutorCorePoolSize = 27;
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_COREPOOLSIZE,
                    Integer.valueOf(jobExecutorCorePoolSize));
            final int jobExecutorMaxPoolSize = 52;
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE,
                    Integer.valueOf(jobExecutorMaxPoolSize));
            final long jobExecutorKeepAliveTime = 559862;
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_KEEPALIVETIME,
                    Long.valueOf(jobExecutorKeepAliveTime));
            final int jobExecutorQueueSize = 123;
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_QUEUESIZE,
                    Integer.valueOf(jobExecutorQueueSize));
            final int jobExecutorMaxTimerJobsPerAcquisition = 682;
            this.jmxClient.setBootstrapAttribute(
                    FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION,
                    Integer.valueOf(jobExecutorMaxTimerJobsPerAcquisition));
            final int jobExecutorMaxAsyncJobsDuePerAcquisition = 357;
            this.jmxClient.setBootstrapAttribute(
                    FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION,
                    Integer.valueOf(jobExecutorMaxAsyncJobsDuePerAcquisition));
            final int jobExecutorAsyncJobAcquireWaitTime = 623;
            this.jmxClient.setBootstrapAttribute(
                    FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME,
                    Integer.valueOf(jobExecutorAsyncJobAcquireWaitTime));
            final int jobExecutorTimerJobAcquireWaitTime = 51397;
            this.jmxClient.setBootstrapAttribute(
                    FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME,
                    Integer.valueOf(jobExecutorTimerJobAcquireWaitTime));
            final int jobExecutorTimerLockTime = 26842;
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_TIMERLOCKTIME,
                    Integer.valueOf(jobExecutorTimerLockTime));
            final int jobExecutorAsyncJobLockTime = 512;
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME,
                    Integer.valueOf(jobExecutorAsyncJobLockTime));

            final boolean engineEnableBpmnValidation = true;
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_ENGINE_ENABLE_BPMN_VALIDATION,
                    String.valueOf(engineEnableBpmnValidation));
            final int defaultFailedJobWaitTime = 15;
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_ENGINE_DEFAULT_FAILED_JOB_WAITTIME,
                    Integer.valueOf(defaultFailedJobWaitTime));
            final int asyncFailedJobWaitTime = 5;
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_ENGINE_ASYNC_FAILED_JOB_WAITTIME,
                    Integer.valueOf(asyncFailedJobWaitTime));

            final String idmEngineConfiguratorClass = FileIdmEngineConfigurator.class.getName();
            this.jmxClient.setBootstrapAttribute(FlowableSEBootstrap.ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CLASS_NAME,
                    idmEngineConfiguratorClass);

            // We continue component lifecycle to be able to check are correctly used
            componentUnderTest.install();
            componentUnderTest.start();

            // Some parameter are set in the Flowable engine configuration
            final ProcessEngine processEngine = flowableComponent.getProcessEngine();
            assertNotNull(processEngine);
            final ProcessEngineConfiguration pec = processEngine.getProcessEngineConfiguration();
            assertNotNull(pec);
            final AsyncExecutor asyncExecutor = pec.getAsyncExecutor();
            assertTrue(asyncExecutor instanceof DefaultAsyncJobExecutor);
            final DefaultAsyncJobExecutor defaultAsyncExecutor = (DefaultAsyncJobExecutor) asyncExecutor;

            assertEquals(databaseType, pec.getDatabaseType());
            assertEquals(databaseSchemaUpdate, pec.getDatabaseSchemaUpdate());

            assertEquals(jdbcDriver, pec.getJdbcDriver());
            assertEquals(jdbcUrl, pec.getJdbcUrl());
            assertEquals(jdbcUsername, pec.getJdbcUsername());
            assertEquals(jdbcPassword, pec.getJdbcPassword());
            assertEquals(jdbcMaxActiveConnections, pec.getJdbcMaxActiveConnections());
            assertEquals(jdbcMaxIdleConnections, pec.getJdbcMaxIdleConnections());
            assertEquals(jdbcMaxCheckoutTime, pec.getJdbcMaxCheckoutTime());
            assertEquals(jdbcMaxWaitTime, pec.getJdbcMaxWaitTime());

            // Job executor is enable when a async executor exists at Flowable level. We use operator '!=' instead of
            // '^' that is more human-readable and equivalent
            // (https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.22.2)
            assertTrue(jobExecutorEnableJobExecutor != (pec.getAsyncExecutor() == null));

            assertEquals(jobExecutorCorePoolSize, defaultAsyncExecutor.getCorePoolSize());
            assertEquals(jobExecutorMaxPoolSize, defaultAsyncExecutor.getMaxPoolSize());
            assertEquals(jobExecutorKeepAliveTime, defaultAsyncExecutor.getKeepAliveTime());
            assertEquals(jobExecutorQueueSize, defaultAsyncExecutor.getQueueSize());
            assertEquals(jobExecutorMaxTimerJobsPerAcquisition, defaultAsyncExecutor.getMaxTimerJobsPerAcquisition());
            assertEquals(jobExecutorMaxAsyncJobsDuePerAcquisition,
                    defaultAsyncExecutor.getMaxAsyncJobsDuePerAcquisition());
            assertEquals(jobExecutorAsyncJobAcquireWaitTime,
                    defaultAsyncExecutor.getDefaultAsyncJobAcquireWaitTimeInMillis());
            assertEquals(jobExecutorTimerJobAcquireWaitTime,
                    defaultAsyncExecutor.getDefaultTimerJobAcquireWaitTimeInMillis());
            assertEquals(jobExecutorTimerLockTime, defaultAsyncExecutor.getTimerLockTimeInMillis());
            assertEquals(jobExecutorAsyncJobLockTime, defaultAsyncExecutor.getAsyncJobLockTimeInMillis());

            assertEquals(engineEnableBpmnValidation,
                    ReflectionHelper.getFieldValue(FlowableSuManager.class,
                            (FlowableSuManager) flowableComponent.getServiceUnitManager(),
                            "enableFlowableBpmnValidation", false));
            assertEquals(defaultFailedJobWaitTime, pec.getDefaultFailedJobWaitTime());
            assertEquals(asyncFailedJobWaitTime, pec.getAsyncFailedJobWaitTime());

        } finally {
            componentUnderTest.delete();
        }
    }
}
