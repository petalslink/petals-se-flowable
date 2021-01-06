/**
 * Copyright (c) 2017-2021 Linagora
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
package org.ow2.petals.flowable.rest;

import java.util.List;

import org.flowable.common.rest.multipart.PutAwareStandardServletMultiPartResolver;
import org.flowable.common.rest.resolver.ContentTypeResolver;
import org.flowable.common.rest.resolver.DefaultContentTypeResolver;
import org.flowable.engine.FormService;
import org.flowable.engine.HistoryService;
import org.flowable.engine.IdentityService;
import org.flowable.engine.ManagementService;
import org.flowable.engine.ProcessEngine;
import org.flowable.engine.RepositoryService;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.TaskService;
import org.flowable.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.flowable.rest.service.api.RestResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.web.multipart.MultipartResolver;
import org.springframework.web.servlet.config.annotation.ContentNegotiationConfigurer;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurationSupport;
import org.springframework.web.servlet.i18n.LocaleChangeInterceptor;
import org.springframework.web.servlet.i18n.SessionLocaleResolver;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * This setup everything needed by Spring to load the Flowable REST API that resides in the packages defined in the
 * {@link ComponentScan} annotation below. Its content is inspired flowable-rest-app code (org.flowable.rest.conf
 * package).
 * 
 * @author Victor NOEL - Linagora
 * @author Jordy CABANNES - Linagora
 */
@Configuration
@ComponentScan({ "org.flowable.rest.exception", "org.flowable.rest.service.api",
        "org.ow2.petals.flowable.rest.config" })
@EnableAsync
public class FlowableProcessApiConfiguration extends WebMvcConfigurationSupport {

    /**
     * Note: this name is used by Flowable code, so we can't change it!
     */
    public static final String FLOWABLE_REST_PROCESS_ENGINE_QUALIFIER = "processEngine";

    /**
     * Note: this name is used by Flowable code, so we can't change it!
     */
    public static final String FLOWABLE_REST_DYNAMIC_BPMN_SERVICE_QUALIFIER = "dynamicBpmnService";

    /**
     * Note: this name is used by Flowable code, so we can't change it!
     */
    public static final String FLOWABLE_REST_IDM_IDENTITY_SERVICE_QUALIFIER = "idmIdentityService";

    /**
     * This is injected by the SE during REST API creation (FlowableSE#createRestApi(ProcessEngineConfigurationImpl)).
     */
    @Autowired
    @Qualifier(FLOWABLE_REST_PROCESS_ENGINE_QUALIFIER)
    protected ProcessEngine processEngine;

    @Bean
    public ProcessEngineConfigurationImpl processEngineConfigurationImpl() {
        return (ProcessEngineConfigurationImpl) processEngine.getProcessEngineConfiguration();
    }

    @Bean
    public ObjectMapper objectMapper() {
        // To avoid instantiating and configuring the mapper everywhere
        return new ObjectMapper();
    }

    @Bean
    public ContentTypeResolver contentTypeResolver() {
        return new DefaultContentTypeResolver();
    }

    @Bean
    public RestResponseFactory processResponseFactory() {
        return new RestResponseFactory(this.objectMapper());
    }

    @Bean
    public SessionLocaleResolver localeResolver() {
        return new SessionLocaleResolver();
    }

    @Bean
    public LocaleChangeInterceptor localeChangeInterceptor() {
        final LocaleChangeInterceptor localeChangeInterceptor = new LocaleChangeInterceptor();
        localeChangeInterceptor.setParamName("language");
        return localeChangeInterceptor;
    }

    @Bean
    public MultipartResolver multipartResolver() {
        return new PutAwareStandardServletMultiPartResolver();
    }

    @Bean
    @Override
    public RequestMappingHandlerMapping requestMappingHandlerMapping() {
        final RequestMappingHandlerMapping requestMappingHandlerMapping = new RequestMappingHandlerMapping();
        requestMappingHandlerMapping.setUseSuffixPatternMatch(false);
        final Object[] interceptors = { localeChangeInterceptor() };
        requestMappingHandlerMapping.setInterceptors(interceptors);
        return requestMappingHandlerMapping;
    }

    @Override
    public void configureMessageConverters(final List<HttpMessageConverter<?>> converters) {
        addDefaultHttpMessageConverters(converters);
        for (final HttpMessageConverter<?> converter : converters) {
            if (converter instanceof MappingJackson2HttpMessageConverter) {
                final MappingJackson2HttpMessageConverter jackson2HttpMessageConverter = (MappingJackson2HttpMessageConverter) converter;
                jackson2HttpMessageConverter.setObjectMapper(this.objectMapper());
                break;
            }
        }
    }

    @Override
    protected void configureContentNegotiation(final ContentNegotiationConfigurer configurer) {
        configurer.favorPathExtension(false);
    }

    @Bean
    public RepositoryService repositoryService() {
        return processEngine.getRepositoryService();
    }

    @Bean
    public RuntimeService runtimeService() {
        return processEngine.getRuntimeService();
    }

    @Bean
    public TaskService taskService() {
        return processEngine.getTaskService();
    }

    @Bean
    public HistoryService historyService() {
        return processEngine.getHistoryService();
    }

    @Bean
    public FormService formService() {
        return processEngine.getFormService();
    }

    @Bean
    public IdentityService identityService() {
        return processEngine.getIdentityService();
    }

    @Bean
    public ManagementService managementService() {
        return processEngine.getManagementService();
    }
}
